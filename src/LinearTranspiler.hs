{-# LANGUAGE PatternSynonyms #-}
module LinearTranspiler (transpileProgram) where

import Control.Monad (join)
import Control.Monad.Identity (Identity)
import Control.Monad.State (StateT, MonadState (get, put), evalStateT)
import LinearCEK (Pattern (..), Simple (..), Term (..), ASTLoc (..), Branch (Branch), File)
import LinearParser (ASTNode (..), ArgumentNode (..), CaseBranch, LiteralOrName (..), PatternNode (..), ASTPos)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Control.Monad.Trans.Reader (runReader)

type TranspilationContext = ()

todo :: a
todo = error "TODO!"

transpileLiteral :: LiteralOrName -> TranspilerResult Simple
transpileLiteral (NumberLiteralNode pos n) = do
  pos <- convertPos pos
  return $ NumberLiteral pos n
transpileLiteral (StringLiteralNode pos s) = do
  pos <- convertPos pos
  return $ StringLiteral pos s
transpileLiteral (UnitLiteral pos) = do
  pos <- convertPos pos
  return $ UnitSimple pos
transpileLiteral (Name pos n) = do
  pos <- convertPos pos
  return $ Variable pos n

getFreeVariablesLiteral :: LiteralOrName -> [String]
getFreeVariablesLiteral (NumberLiteralNode _ _) = []
getFreeVariablesLiteral (StringLiteralNode _ _) = []
getFreeVariablesLiteral (Name _ n) = [n]
getFreeVariablesLiteral (UnitLiteral _) = []

merge :: (Eq a) => [a] -> [a] -> [a]
merge as1 as2 = as1 ++ (as2 `minus` as1)

minus :: (Eq a) => [a] -> [a] -> [a]
minus as1 as2 = filter (`notElem` as2) as1

getFreeVariablesPat :: PatternNode -> [String]
getFreeVariablesPat (WildCardPatternNode _ s) = [s]
getFreeVariablesPat (ConstructorPatternNode _ _ patterns) = join (map getFreeVariablesPat patterns)
getFreeVariablesPat (TuplePatternNode _ patterns) = join (map getFreeVariablesPat patterns)
getFreeVariablesPat (AsPatternNode _ name pat) = name : getFreeVariablesPat pat

getFreeVariablesArgument :: ArgumentNode -> [String]
getFreeVariablesArgument (MoveArg lit) = getFreeVariablesLiteral lit
getFreeVariablesArgument (SplitArg lit) = getFreeVariablesLiteral lit

getFreeVariablesCase :: CaseBranch -> [String]
getFreeVariablesCase (_, pat, body) = getFreeVariables body `minus` getFreeVariablesPat pat

getFreeVariables :: ASTNode -> [String]
getFreeVariables (LetPatternNode _ pat val body) = getFreeVariables val `merge` (getFreeVariables body `minus` getFreeVariablesPat pat)
getFreeVariables (LetFunctionNode _ name params _ funcBody body) = (getFreeVariables funcBody `minus` map fst params) `merge` (getFreeVariables body `minus` [name])
getFreeVariables (LetRecFunctionNode _ name params _ funcBody body) = (getFreeVariables funcBody `minus` (name : map fst params)) `merge` (getFreeVariables body `minus` [name])
getFreeVariables (Parenthesis _ node) = getFreeVariables node
getFreeVariables (TupleNode _ vals) = foldr (merge . getFreeVariablesLiteral) [] vals
getFreeVariables (RebalanceNode _ vals) = foldr (merge . getFreeVariablesLiteral) [] vals
getFreeVariables (ListNode _ vals) = foldr (merge . getFreeVariablesLiteral) [] vals
getFreeVariables (RefNode _ val) = getFreeVariablesLiteral val
getFreeVariables (SetRefNode _ ref val) = getFreeVariablesLiteral ref `merge` getFreeVariablesLiteral val
getFreeVariables (DerefNode _ ref) = getFreeVariablesLiteral ref
getFreeVariables (MoveNode _ ref) = getFreeVariablesLiteral ref
getFreeVariables (SplitNode _ ref) = getFreeVariablesLiteral ref
getFreeVariables (DropNode _ ref) = getFreeVariablesLiteral ref
getFreeVariables (ThunkNode _ thunk) = getFreeVariables thunk
getFreeVariables (MergeNode _ ref1 ref2) = getFreeVariablesLiteral ref1 `merge` getFreeVariablesLiteral ref2
getFreeVariables (SemicolonNode _ node1 node2) = getFreeVariables node1 `merge` getFreeVariables node2
getFreeVariables (LiteralNode _ lit) = getFreeVariablesLiteral lit
getFreeVariables (CaptureNode _ lit) = getFreeVariablesLiteral lit
getFreeVariables (TupleIndexNode _ tuple index) = getFreeVariablesLiteral tuple `merge` getFreeVariablesLiteral index
getFreeVariables (UnClosNode _ clos holders) = getFreeVariablesLiteral clos `merge` foldr (merge . getFreeVariablesLiteral) [] holders
getFreeVariables (ApplicationNode _ rator rands) = getFreeVariablesLiteral rator `merge` foldr (merge . getFreeVariablesArgument) [] rands
getFreeVariables (ParallelNode _ branches) = foldr (merge . getFreeVariables) [] branches
getFreeVariables (DataNode _ _ _ constructors body) = getFreeVariables body `minus` map fst constructors
getFreeVariables (CaseNode _ target cases) = getFreeVariablesLiteral target `merge` foldr (merge . getFreeVariablesCase) [] cases
getFreeVariables (AcquireNode _ targetVal asName body) = getFreeVariablesLiteral targetVal `merge` (getFreeVariables body `minus` [asName])
getFreeVariables (AbstractNode _ lit) = getFreeVariablesLiteral lit
getFreeVariables (UnabstractNode _ lit) = getFreeVariablesLiteral lit

transpileWildcardPattern :: PatternNode -> String
transpileWildcardPattern (WildCardPatternNode _ s) = s
transpileWildcardPattern notWildCardPat = error "Unsupported nested pattern: " ++ show notWildCardPat

transpilePattern :: PatternNode -> TranspilerResult Pattern
transpilePattern (WildCardPatternNode pos s) = do
  loc <- convertPos pos
  return $ WildCardPattern loc s
transpilePattern (ConstructorPatternNode pos constructor patterns) = do
  loc <- convertPos pos
  return $ ConstructorPattern loc constructor (map transpileWildcardPattern patterns)
transpilePattern (TuplePatternNode pos patterns) = do
  loc <- convertPos pos
  return $ ConstructorPattern loc ("Tuple:" ++ show (length patterns)) (map transpileWildcardPattern patterns)
transpilePattern (AsPatternNode loc name pat) = error "As patterns are not supported"

type TranspilerResult a = StateT Int (ReaderT File Identity) a

tupleHelper :: [a] -> String
tupleHelper as = "Tuple:" ++ show (length as)

pattern LetName :: ASTPos -> String -> ASTNode -> ASTNode -> ASTNode
pattern LetName pos name val body <- LetPatternNode pos (WildCardPatternNode undefined name) val body where
  LetName pos name val body = LetPatternNode pos (WildCardPatternNode undefined name) val body


genTempName :: TranspilerResult String
genTempName = do
  val <- get
  put (1 + val)
  return $ "__genned__name:" ++ show val

convertPos :: ASTPos -> TranspilerResult ASTLoc
convertPos pos = do
  srcFile <- ask
  return $ ASTLoc pos srcFile

transpile :: ASTNode -> TranspilerResult Term
transpile (LetName pos name (DerefNode _ rawRef) body) = do
  ref <- transpileLiteral rawRef
  body <- transpile body
  pos <- convertPos pos
  return $ DerefIn pos name ref body
transpile (LetPatternNode pos rawPat val body) = do
  pat <- transpilePattern rawPat
  val <- transpile val
  body <- transpile body
  pos <- convertPos pos
  return $ LetPattern pos pat val body
transpile (LetFunctionNode pos name params returnType funcBody body) = do
  let freeVars = getFreeVariables funcBody `minus` map fst params
  funcBody <- transpile funcBody
  body <- transpile body
  pos <- convertPos pos
  return $ LetFunction pos name freeVars params returnType funcBody body
transpile (LetRecFunctionNode pos name params returnType funcBody body) = do
  let freeVars = getFreeVariables funcBody `minus` (name:map fst params)
  funcBody <- transpile funcBody
  body <- transpile body
  pos <- convertPos pos
  return $ LetRecFunction pos name freeVars params returnType funcBody body
transpile (Parenthesis _ node) = transpile node
transpile (TupleNode pos vals) = do
  pos <- convertPos pos
  vals <- mapM transpileLiteral vals
  return $ Application pos (Variable pos (tupleHelper vals)) vals
transpile (RebalanceNode pos vals) = do
  pos <- convertPos pos
  vals <- mapM transpileLiteral vals
  return $ Rebalance pos vals
transpile (ListNode _ vals) = error "List literal syntax currently unsupported"
transpile (RefNode pos val) = do
  pos <- convertPos pos
  val <- transpileLiteral val
  return $ RefExpr pos val
transpile (SetRefNode pos ref val) = do
  pos <- convertPos pos
  ref <- transpileLiteral ref
  val <- transpileLiteral val
  return $ SetRef pos ref val
transpile (DerefNode _ ref) = error "Deref's are only supported in single let bindings (ex: :let x = SOME_REF in)"
transpile (MoveNode pos ref) = do
  pos <- convertPos pos
  ref <- transpileLiteral ref
  return $ Move pos ref
transpile (SplitNode pos ref) = do
  pos <- convertPos pos
  ref <- transpileLiteral ref
  return $ Split pos ref
transpile (DropNode pos ref) = do
  pos <- convertPos pos
  ref <- transpileLiteral ref
  return $ Drop pos ref
transpile (ThunkNode pos thunk) = do
  let freeVars = getFreeVariables thunk
  thunk <- transpile thunk
  pos <- convertPos pos
  return $ Thunk pos freeVars thunk
transpile (MergeNode pos ref1 ref2) = do
  pos <- convertPos pos
  ref1 <- transpileLiteral ref1
  ref2 <- transpileLiteral ref2
  return $ Merge pos ref1 ref2
transpile (SemicolonNode pos node1 node2) = do
  node1 <- transpile node1
  node2 <- transpile node2
  temp <- genTempName
  pos <- convertPos pos
  return $ LetPattern pos (WildCardPattern pos temp) node1 (Semicolon pos (Drop pos (Variable pos temp)) node2)
  -- return $ Semicolon node1 node2
transpile (LiteralNode pos lit) = do
  pos <- convertPos pos
  lit <- transpileLiteral lit
  return $ Literal pos lit
transpile (CaptureNode _ _) = error "CaptureNode not allowed"
transpile (TupleIndexNode _ tuple index) = error "Tuple indexing notation is currently unsupported"
transpile (UnClosNode pos clos holders) = do
  pos <- convertPos pos
  holders <- mapM transpileLiteral holders
  clos <- transpileLiteral clos
  return $ UnClos pos holders clos
transpile (ApplicationNode pos rator rands) = do
  pos <- convertPos pos
  rator <- transpileLiteral rator
  rands <- mapM transpileArg rands
  return $ Application pos rator rands
transpile (ParallelNode pos branches) = do
  branches <- mapM transpile branches
  pos <- convertPos pos
  return $ Parallel pos branches
transpile (DataNode pos typeName qNames constructors body) = do
  body <- transpile body
  pos <- convertPos pos
  return $ DataTypeDeclaration pos typeName qNames constructors body
transpile (CaseNode pos target cases) = do
  cases <- mapM transpileCase cases
  pos <- convertPos pos
  target <- transpileLiteral target
  return $ Case pos target cases
transpile (AcquireNode pos targetVal asName body) = do
  body <- transpile body
  pos <- convertPos pos
  targetVal <- transpileLiteral targetVal
  return $ Acquire pos targetVal asName body
transpile (AbstractNode pos lit) = do
  pos <- convertPos pos
  lit <- transpileLiteral lit
  return $ Abstract pos lit
transpile (UnabstractNode pos lit) = do
  pos <- convertPos pos
  lit <- transpileLiteral lit
  return $ Unabstract pos lit

transpileCase :: CaseBranch -> TranspilerResult Branch
transpileCase (pos, pat, body) = do
  body <- transpile body
  loc <- convertPos pos
  pat <- transpilePattern pat
  return (Branch loc pat body)

transpileArg :: ArgumentNode -> TranspilerResult Simple
transpileArg (SplitArg e) = transpileLiteral e
transpileArg (MoveArg _) = error "Explicit Move args are unsupported"

transpileProgram :: File -> ASTNode -> Term
transpileProgram file node = runReader (evalStateT (transpile node) 0) file