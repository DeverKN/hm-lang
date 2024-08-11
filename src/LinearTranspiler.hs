{-# LANGUAGE PatternSynonyms #-}
module LinearTranspiler (transpileProgram, TypeTranspilerError(..)) where

import Control.Monad (join)
import Control.Monad.Identity (Identity)
import Control.Monad.State (StateT, MonadState (get, put), evalStateT)
import LinearCEK (Pattern (..), Simple (..), Term (..), ASTLoc (..), Branch (Branch), File)
import LinearParser (ASTNode (..), ArgumentNode (..), CaseBranch, LiteralOrName (..), PatternNode (..), ASTPos)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Control.Monad.Trans.Reader (runReader)
import Types (Type (..), RawType (..), substAliasType, VarName, pattern TupleConstructor, pattern FreshVarName)
import Pretty (Pretty(pretty))
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Except (runExceptT)

data TypeDescriptor =
  DataType [String]
  | PrimitiveType
  | AliasType [VarName] Type
  deriving (Show)

instance Pretty TypeDescriptor where
  pretty = show

type TypeEnv = [(String, TypeDescriptor)]

baseTypeEnv :: [(String, TypeDescriptor)]
baseTypeEnv = [
  ("Number", PrimitiveType),
  ("String", PrimitiveType),
  ("Tuple", DataType ["a", "b"]),
  ("List", DataType ["a"])]

data TypeTranspilerError =
  UnknownType ASTLoc String
  | WrongArity ASTLoc RawType Int [RawType]
  deriving (Show)

-- type TypeTranspilerResult a = Either TypeTranspilerError a

lookupTy :: ASTLoc -> TypeEnv -> String -> TranspilerResult TypeDescriptor
lookupTy loc env (TupleConstructor n) = return $ DataType (map (\n -> "x" ++ show n) [1..n])
lookupTy loc env name = case lookup name env of
                    (Just descriptor) -> return descriptor
                    Nothing -> throwError (UnknownType loc name)

transpileType :: ASTLoc -> TypeEnv -> RawType -> TranspilerResult Type
transpileType loc env ty@(RawTyCon s) = do
  descriptor <- lookupTy loc env s
  case descriptor of
    DataType [] -> return (TyCon s)
    AliasType [] aliasedTy -> return (TyAlias ty aliasedTy)
    PrimitiveType -> return (TyCon s)
    AliasType args _ -> throwError (WrongArity loc ty (length args) [])
    DataType args -> throwError (WrongArity loc ty (length args) [])
transpileType loc _ (RawTyVar n) = return (TyVar n)
transpileType loc tyEnv (RawTyArrow env params rType) = do
  params <- mapM (transpileType loc tyEnv) params
  rType <- transpileType loc tyEnv rType
  return (TyArrow env params rType)
transpileType loc tyEnv rawTy@(RawTyApp rawTyRator@(RawTyCon rator) rawTyRands) = do
  ratorDescriptor <- lookupTy loc tyEnv rator
  tyRands <- mapM (transpileType loc tyEnv) rawTyRands
  case ratorDescriptor of
    PrimitiveType -> throwError (WrongArity loc rawTyRator 0 rawTyRands)
    DataType params | length params == length tyRands -> return (TyApp (TyCon rator) tyRands)
    DataType params -> throwError (WrongArity loc rawTyRator (length params) rawTyRands)
    AliasType params ty | length params == length tyRands -> return (TyAlias rawTy (substAliasType (zip params tyRands) ty))
    AliasType params ty -> throwError (WrongArity loc rawTyRator (length params) rawTyRands)
transpileType loc tyEnv ty@(RawTyApp {}) = error $ "Invalid type application: " ++ pretty ty
transpileType loc tyEnv (RawTySchema qNames ty) = do
  ty <- transpileType loc tyEnv ty
  return (TySchema qNames ty)
transpileType loc tyEnv (RawTyExists qNames ty) = do
  ty <- transpileType loc tyEnv ty
  return (TyExists qNames ty)
transpileType loc tyEnv (RawTyFrac frac addr ty) = do
  ty <- transpileType loc tyEnv ty
  return (TyFrac frac addr ty)



-- type TranspilationContext = ()

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
getFreeVariables (UnClosNode _ clos) = getFreeVariablesLiteral clos --`merge` foldr (merge . getFreeVariablesLiteral) [] holders
getFreeVariables (ApplicationNode _ rator rands) = getFreeVariablesLiteral rator `merge` foldr (merge . getFreeVariablesArgument) [] rands
getFreeVariables (ParallelNode _ branches) = foldr (merge . getFreeVariables) [] branches
getFreeVariables (DataNode _ _ _ constructors body) = getFreeVariables body `minus` map fst constructors
getFreeVariables (CaseNode _ target cases) = getFreeVariablesLiteral target `merge` foldr (merge . getFreeVariablesCase) [] cases
getFreeVariables (AcquireNode _ targetVal asName body) = getFreeVariablesLiteral targetVal `merge` (getFreeVariables body `minus` [asName])
getFreeVariables (AbstractNode _ lit) = getFreeVariablesLiteral lit
getFreeVariables (UnabstractNode _ lit) = getFreeVariablesLiteral lit
getFreeVariables (TypeSynonymNode _ _ _ _ body) = getFreeVariables body

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

type TranspilerResult a = ExceptT TypeTranspilerError (StateT Int (ReaderT File Identity)) a

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

mapM2 :: Monad m => (b -> m c) -> [(a, b)] -> m [(a, c)]
mapM2 f = mapM (\(a, b) -> f b >>= \c -> return (a, c))

transpileParams :: ASTLoc -> TypeEnv -> [(String, RawType)] -> TranspilerResult [(String, Type)]
transpileParams loc tyEnv = mapM2 (transpileType loc tyEnv)

transpileConstructors :: ASTLoc -> TypeEnv -> [(String, [RawType])] -> TranspilerResult [(String, [Type])]
transpileConstructors loc tyEnv = mapM2 (mapM (transpileType loc tyEnv))

transpile :: TypeEnv -> ASTNode -> TranspilerResult Term
transpile tyEnv (LetName pos name (DerefNode _ rawRef) body) = do
  ref <- transpileLiteral rawRef
  body <- transpile tyEnv body
  pos <- convertPos pos
  return $ DerefIn pos name ref body
transpile tyEnv (LetPatternNode pos rawPat val body) = do
  pat <- transpilePattern rawPat
  val <- transpile tyEnv val
  body <- transpile tyEnv body
  pos <- convertPos pos
  return $ LetPattern pos pat val body
transpile tyEnv (LetFunctionNode pos name params returnType funcBody body) = do
  let freeVars = getFreeVariables funcBody `minus` map fst params
  funcBody <- transpile tyEnv funcBody
  body <- transpile tyEnv body
  pos <- convertPos pos
  params <- transpileParams pos tyEnv params
  returnType <- transpileType pos tyEnv returnType
  return $ LetFunction pos name freeVars params returnType funcBody body
transpile tyEnv (LetRecFunctionNode pos name params returnType funcBody body) = do
  let freeVars = getFreeVariables funcBody `minus` (name:map fst params)
  funcBody <- transpile tyEnv funcBody
  body <- transpile tyEnv body
  pos <- convertPos pos
  params <- transpileParams pos tyEnv params
  returnType <- transpileType pos tyEnv returnType
  return $ LetRecFunction pos name freeVars params returnType funcBody body
transpile tyEnv (Parenthesis _ node) = transpile tyEnv node
transpile tyEnv (TupleNode pos vals) = do
  pos <- convertPos pos
  vals <- mapM transpileLiteral vals
  return $ Application pos (Variable pos (tupleHelper vals)) vals
transpile tyEnv (RebalanceNode pos vals) = do
  pos <- convertPos pos
  vals <- mapM transpileLiteral vals
  return $ Rebalance pos vals
transpile tyEnv (ListNode _ vals) = error "List literal syntax currently unsupported"
transpile tyEnv (RefNode pos val) = do
  pos <- convertPos pos
  val <- transpileLiteral val
  return $ RefExpr pos val
transpile tyEnv (SetRefNode pos ref val) = do
  pos <- convertPos pos
  ref <- transpileLiteral ref
  val <- transpileLiteral val
  return $ SetRef pos ref val
transpile tyEnv (DerefNode _ ref) = error "Deref's are only supported in single let bindings (ex: :let x = SOME_REF in)"
transpile tyEnv (MoveNode pos ref) = do
  pos <- convertPos pos
  ref <- transpileLiteral ref
  return $ Move pos ref
transpile tyEnv (SplitNode pos ref) = do
  pos <- convertPos pos
  ref <- transpileLiteral ref
  return $ Split pos ref
transpile tyEnv (DropNode pos ref) = do
  pos <- convertPos pos
  ref <- transpileLiteral ref
  return $ Drop pos ref
transpile tyEnv (ThunkNode pos thunk) = do
  let freeVars = getFreeVariables thunk
  thunk <- transpile tyEnv thunk
  pos <- convertPos pos
  return $ Thunk pos freeVars thunk
transpile tyEnv (MergeNode pos ref1 ref2) = do
  pos <- convertPos pos
  ref1 <- transpileLiteral ref1
  ref2 <- transpileLiteral ref2
  return $ Merge pos ref1 ref2
transpile tyEnv (SemicolonNode pos node1 node2) = do
  node1 <- transpile tyEnv node1
  node2 <- transpile tyEnv node2
  temp <- genTempName
  pos <- convertPos pos
  return $ LetPattern pos (WildCardPattern pos temp) node1 (Semicolon pos (Drop pos (Variable pos temp)) node2)
  -- return $ Semicolon node1 node2
transpile tyEnv (LiteralNode pos lit) = do
  pos <- convertPos pos
  lit <- transpileLiteral lit
  return $ Literal pos lit
transpile tyEnv (CaptureNode _ _) = error "CaptureNode not allowed"
transpile tyEnv (TupleIndexNode _ tuple index) = error "Tuple indexing notation is currently unsupported"
transpile tyEnv (UnClosNode pos clos) = do
  pos <- convertPos pos
  -- holders <- mapM transpileLiteral holders
  clos <- transpileLiteral clos
  return $ UnClos pos clos
transpile tyEnv (ApplicationNode pos rator rands) = do
  pos <- convertPos pos
  rator <- transpileLiteral rator
  rands <- mapM transpileArg rands
  return $ Application pos rator rands
transpile tyEnv (ParallelNode pos branches) = do
  branches <- mapM (transpile tyEnv) branches
  pos <- convertPos pos
  return $ Parallel pos branches
transpile tyEnv (DataNode pos typeName qNames constructors body) = do
  body <- transpile ((typeName, DataType qNames):tyEnv) body
  pos <- convertPos pos
  constructors <- transpileConstructors pos tyEnv constructors
  return $ DataTypeDeclaration pos typeName qNames constructors body
transpile tyEnv (CaseNode pos target cases) = do
  cases <- mapM (transpileCase tyEnv) cases
  pos <- convertPos pos
  target <- transpileLiteral target
  return $ Case pos target cases
transpile tyEnv (AcquireNode pos targetVal asName body) = do
  body <- transpile tyEnv body
  pos <- convertPos pos
  targetVal <- transpileLiteral targetVal
  return $ Acquire pos targetVal asName body
transpile tyEnv (AbstractNode pos lit) = do
  pos <- convertPos pos
  lit <- transpileLiteral lit
  return $ Abstract pos lit
transpile tyEnv (UnabstractNode pos lit) = do
  pos <- convertPos pos
  lit <- transpileLiteral lit
  return $ Unabstract pos lit
transpile tyEnv (TypeSynonymNode pos name qNames ty body) = do
  loc <- convertPos pos
  ty <- transpileType loc tyEnv ty
  transpile ((name, AliasType (map FreshVarName qNames) ty):tyEnv) body-- do
  -- pos <- convertPos loc
  -- body <- transpile tyEnv body
  -- return $ TypeSynonym pos name qNames ty body

transpileCase :: TypeEnv -> CaseBranch -> TranspilerResult Branch
transpileCase tyEnv (pos, pat, body) = do
  body <- transpile tyEnv body
  loc <- convertPos pos
  pat <- transpilePattern pat
  return (Branch loc pat body)

transpileArg :: ArgumentNode -> TranspilerResult Simple
transpileArg (SplitArg e) = transpileLiteral e
transpileArg (MoveArg _) = error "Explicit Move args are unsupported"

transpileProgram :: File -> ASTNode -> Either TypeTranspilerError Term
transpileProgram file node = runReader (evalStateT (runExceptT (transpile baseTypeEnv node)) 0) file