{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module LinearTypeChecker where

import Control.Monad (foldM, replicateM, unless, when)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.RWS (MonadState (..))
import Control.Monad.State (StateT (runStateT), evalState, evalStateT)
import Control.Monad.State.Lazy (runState)
import Data.Functor ((<&>))
import Data.List (delete, deleteBy, union, intercalate)
import LinearCEK (Pattern (..), Simple (..), Term (..), ASTLoc (ASTLoc), HasLoc (getLoc), Branch (Branch), File (..), printLoc, fancyPrintLoc)
import LinearParser (parseFile)
import LinearTranspiler (transpileProgram)
import Pretty (Pretty (pretty))
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Types (AbstractAddress (..), AbstractFrac (..), CaptureEnv (..), Type (..), UnificationResult, arity, eqType, genQName, instantiate, number, paramTypes, returnType, tuple, unify, (|*|), (|+|), (|/|), pattern AbstractNum, pattern AbstractVar, pattern CooperativeRef, pattern CooperativeRefType, pattern Ref, pattern Tuple, pattern Unit, AbstractExpr (Var), constructorFieldTypes, getQuantifiedVars, OverallUnificationResult, UnificationError, pattern FreshVarName, VarName, genQName')
import qualified Types as UnificationError(UnificationError(..))
import Debug.Trace (traceM)
import Data.Foldable (forM_)
import Data.Either (fromLeft)
import Control.Monad (void)
import GHC.Stack (HasCallStack)
-- import LinearTypeChecker (TupleConstructor)

-- import Test.HUnit

todo :: a
todo = error "TODO"

data TypeCheckerError
  = TypeMismatch ASTLoc Type Type
  | UnificationError ASTLoc Type Type Type Type
  | AddressMismatch ASTLoc AbstractAddress AbstractAddress
  | UnboundName ASTLoc String
  | InferenceError ASTLoc Term
  | NotArrowType ASTLoc Type Term
  | NotARef ASTLoc Type Simple
  | NotAFunction ASTLoc Type Simple
  | LinearVarReused ASTLoc String
  | LinearVarNotUsed ASTLoc String TypeCheckerEnv
  | NonEmptyEnv ASTLoc TypeCheckerEnv
  | UnknownConstructor ASTLoc String
  | NotAConstructor ASTLoc String Type
  | CaseConstructorMismatch ASTLoc Type Type
  | CaseBodyTypeMismatch ASTLoc Term [Type]
  -- | CaseBodyUsageMismatch ASTLoc String
  -- | CaseBodyEnvTypeMismatch ASTLoc String
  | CaseBodyEnvMismatch ASTLoc Term EnvMismatchResult--[(ASTLoc, TypeCheckerEnv)]
  | EmptyCase ASTLoc Term
  | InvalidDrop ASTLoc Simple AbstractFrac
  | SetPartialRef ASTLoc Simple AbstractFrac
  | CooperatePartialRef ASTLoc Simple AbstractFrac
  | SharePartialRef ASTLoc Simple AbstractFrac
  | DeRefEmptyRef ASTLoc Simple
  | DropClosure ASTLoc Simple
  | NotAClosure ASTLoc Type Simple
  | UnClosPolyEnv ASTLoc Simple CaptureEnv
  | UnClosPartial ASTLoc Simple
  | DerefInConsumed ASTLoc
  | EmptyRebalance ASTLoc
  | CircularUnification ASTLoc
  | InvalidDropType ASTLoc Type
  | InvalidAbstract ASTLoc Type
  deriving (Show)

hideBaseBindings :: TypeCheckerEnv -> TypeCheckerEnv
hideBaseBindings env = case evalStateT (elimBindings undefined baseTypeBindings env) 0 of
                          Right env -> env

data EnvMismatchResult = MismatchedUse String ASTLoc ASTLoc
                        | MismatchedType String (ASTLoc, Type) (ASTLoc, Type) UnificationError
                        deriving (Show)

convertCaseBodyMismatchHelper2 :: ASTLoc -> (String, Type) -> ASTLoc -> [(String, Type)] -> Either EnvMismatchResult ()
convertCaseBodyMismatchHelper2 firstBranchLoc (varName, varTy) otherBranchLoc otherBranchEnv =
  case lookup varName otherBranchEnv of
    Nothing -> Left (MismatchedUse varName otherBranchLoc firstBranchLoc)
    Just otherTy ->
      case varTy `unify` otherTy of
        (Left (_, _, err)) -> Left (MismatchedType varName (firstBranchLoc, varTy) (otherBranchLoc, otherTy) err)
        (Right _) -> pure ()
        -- pure () 
        -- Left (MismatchedType varName (firstBranchLoc, varTy) (otherBranchLoc, otherTy))

convertCaseBodyMismatchHelper :: ASTLoc -> [(String, Type)] -> ASTLoc -> [(String, Type)] -> Either EnvMismatchResult ()
convertCaseBodyMismatchHelper firstBranchLoc firstBranchEnv otherBranchLoc otherBranchEnv = forM_ firstBranchEnv (\var -> convertCaseBodyMismatchHelper2 firstBranchLoc var otherBranchLoc otherBranchEnv)

forEachAndRest :: (a -> [a] -> b) -> [a] -> [b]
forEachAndRest f as = helper as []
                    where
                      helper [] _ = []
                      helper [first] previous = [f first previous]
                      helper (first:rest) previous = f first (previous ++ rest):helper rest (first:previous)

-- listMtoMList :: Monad m => [m a] -> m [a]
-- listMtoMList ms = do
--   x <- ms
--   x

forEachAndRestM :: Monad m => (a -> [a] -> m b) -> [a] -> m [b]
forEachAndRestM f as = sequence $ forEachAndRest f as

forEachAndRestM_ :: Monad f => (a -> [a] -> f b) -> [a] -> f ()
forEachAndRestM_ f as = void $ forEachAndRestM f as

convertCaseBodyMismatch :: [(ASTLoc, TypeCheckerEnv)] -> Either EnvMismatchResult ()
-- convertCaseBodyMismatch [] = return ()
convertCaseBodyMismatch = forEachAndRestM_ helper
  where helper (firstLoc, (firstEnv, _)) rest = do
                                          let restLocs = map fst rest
                                          let restEnvs = map (fst . snd) rest
                                          forM_ (zip restLocs restEnvs) (uncurry (convertCaseBodyMismatchHelper firstLoc firstEnv))
                                          convertCaseBodyMismatch rest

-- [
--   [
--     (dropList, ∀ a1 env. Frac<1, 1 * ([] | ∃ addr3. Frac<addr3, 1 * ((<#con>List ∃ addr5. Frac<addr5, 1 * ((∃ addr0. Frac<addr0, 1 * (<#fixed>env | <#con>a0 -> Unit)>, Unit))>))> -> ∃ addr4. Frac<addr4, 1 * (<#var-env>env | ∃ addr5. Frac<addr5, 1 * ((∃ addr0. Frac<addr0, 1 * (<#fixed>env | <#con>a0 -> Unit)>, Unit))> -> Unit)> -> ∃ addr1. Frac<addr1, 1 * ((∃ addr2. Frac<addr2, 1 * (<#var-env>env | ∃ addr5. Frac<addr5, 1 * ((∃ addr0. Frac<addr0, 1 * (<#fixed>env | <#con>a0 -> Unit)>, Unit))> -> Unit)>, Unit))>)>),
--     (Cons, ∀ a. Frac<0, 1 * ([] | (var a) -> ∃ addr. Frac<addr, 1 * ((<#con>List (var a)))> -> ∃ addr. Frac<addr, 1 * ((<#con>List (var a)))>)>),(Nil, ∀ a. Frac<0, 1 * ([] | Unit -> ∃ addr. Frac<addr, 1 * ((<#con>List (var a)))>)>),(+, ∀ a b. Frac<b, a * ([] | <#con>Number -> <#con>Number -> <#con>Number)>),(-, ∀ a b. Frac<b, a * ([] | <#con>Number -> <#con>Number -> <#con>Number)>),(*, ∀ a b. Frac<b, a * ([] | <#con>Number -> <#con>Number -> <#con>Number)>),(/, ∀ a b. Frac<b, a * ([] | <#con>Number -> <#con>Number -> <#con>Number)>)],
--   [(dropList, ∀ a env a. Frac<1, 1 * ([] | ∃ addr. Frac<addr, 1 * ((<#con>List (var a)))> -> ∃ addr. Frac<addr, 1 * (<#var-env>env | (var a) -> Unit)> -> ∃ addr. Frac<addr, 1 * ((∃ addr. Frac<addr, 1 * (<#var-env>env | (var a) -> Unit)>, Unit))>)>),
--   (Cons, ∀ a. Frac<0, 1 * ([] | (var a) -> ∃ addr. Frac<addr, 1 * ((<#con>List (var a)))> -> ∃ addr. Frac<addr, 1 * ((<#con>List (var a)))>)>),(Nil, ∀ a. Frac<0, 1 * ([] | Unit -> ∃ addr. Frac<addr, 1 * ((<#con>List (var a)))>)>),
--   (+, ∀ a b. Frac<b, a * ([] | <#con>Number -> <#con>Number -> <#con>Number)>),(-, ∀ a b. Frac<b, a * ([] | <#con>Number -> <#con>Number -> <#con>Number)>),
--   (*, ∀ a b. Frac<b, a * ([] | <#con>Number -> <#con>Number -> <#con>Number)>),(/, ∀ a b. Frac<b, a * ([] | <#con>Number -> <#con>Number -> <#con>Number)>)]]
-- convertCaseBodyMismatch ((firstLoc, (firstEnv, _)) : rest) = do
--   let restLocs = map fst rest
--   let restEnvs = map (fst . snd) rest
--   forM_ (zip restLocs restEnvs) (uncurry (convertCaseBodyMismatchHelper firstLoc firstEnv))
--   convertCaseBodyMismatch rest

makeCaseBodyMismatchMsg :: [(ASTLoc, TypeCheckerEnv)] -> String
makeCaseBodyMismatchMsg bodies = case fromLeft (error $ "case body mismatch has no errors for bodies: " ++ pretty (map (fst . snd) bodies)) (convertCaseBodyMismatch bodies) of
                                    MismatchedUse varName usedBranch unusedBranch -> varName ++ " was used in this branch:\n" ++ fancyPrintLoc usedBranch ++ "\nbut not this branch:\n" ++ fancyPrintLoc unusedBranch
                                    MismatchedType varName (branch1, ty1) (branch2, ty2) err -> varName ++ " has type " ++ pretty ty1 ++ " in this branch:\n" ++ fancyPrintLoc branch1 ++ "\nBut type " ++ pretty ty2 ++ " in this branch:\n" ++ fancyPrintLoc branch2 ++ "These aren't the same because:\n" ++ pretty err
                                    -- catch -> undefined
                                     --case (convertCaseBodyMismatch bodies) of

                                  -- Left (MismatchedUse consumedVarName consumedBranch, notConsumedBranch) ->

                                  -- Left (MismatchedType consumedVarName (branch1, ty1), (branch2, ty2)) ->
                                  -- Left (MismatchedType { consumedVarName, consumedBranch, notConsumedBranch})

instance Pretty EnvMismatchResult where
  pretty (MismatchedUse varName usedBranch unusedBranch) = varName ++ " was used in this branch:\n" ++ fancyPrintLoc usedBranch ++ "\nbut not this branch:\n" ++ fancyPrintLoc unusedBranch
  pretty (MismatchedType varName (branch1, ty1) (branch2, ty2) err) = varName ++ " has type " ++ pretty ty1 ++ " in this branch:\n" ++ fancyPrintLoc branch1 ++ "\nBut type " ++ pretty ty2 ++ " in this branch:\n" ++ fancyPrintLoc branch2 ++ "These aren't the same because:\n" ++ pretty err

instance Pretty TypeCheckerError where
  pretty :: TypeCheckerError -> String
  pretty (TypeMismatch loc ty1 ty2) = "Type mismatch, expected: " ++ pretty ty1 ++ " but got " ++ pretty ty2 ++ " Here: \n" ++ fancyPrintLoc loc
  pretty (UnificationError loc ty1 ty2 innerTy1 innerTy2) = "Type mismatch, expected: " ++ pretty ty1 ++ " but got " ++ pretty ty2 ++ ". Couldn't unify " ++ pretty innerTy1 ++ " with " ++ pretty innerTy2 ++ " Here:\n" ++ fancyPrintLoc loc
  pretty (CaseBodyTypeMismatch loc term types) = "Case body types don't match got: " ++ pretty types ++ " Here: \n" ++ fancyPrintLoc loc
  pretty (CaseBodyEnvMismatch loc term err) = pretty err --"Case body environments don't match got: " ++ intercalate "\n" (map (pretty . fst . hideBaseBindings) envs) ++ " at " ++ fancyPrintLoc loc
  pretty (InvalidAbstract loc ty) = "Cannot abstract: " ++ pretty ty ++ " since it's not a fractional type. Error here: \n" ++ fancyPrintLoc loc
  pretty (LinearVarNotUsed loc varName env) = "Variable " ++ varName ++ " was not used. Env is: " ++ pretty env ++ ". Issue occurs here:\n" ++ fancyPrintLoc loc
  pretty (NonEmptyEnv loc env) = "Final environment isn't empty, the following variables are unused: " ++ pretty env ++ ". Problem occurs here:\n" ++ fancyPrintLoc loc
  pretty (UnboundName loc name) = "Unbound variable: " ++ name ++ " used here:\n" ++ fancyPrintLoc loc
  pretty (InvalidDropType loc ty) = "Cannot drop variable of type: " ++ pretty ty ++ " here:\n" ++ fancyPrintLoc loc
  -- pretty (UnificationErr loc ty1 ty2) = "Type mismatch, expected: " ++ pretty ty2 ++ " but got " ++ pretty ty1 ++ " at " ++ printLoc loc
  pretty x = show x

type TypeCheckerEnv = ([(String, Type)], [VarName])

type TypeCheckerState = Int

type TypeCheckerStateT m a = StateT TypeCheckerState m a

type TypeCheckerResult a = TypeCheckerStateT (Either TypeCheckerError) a

checkSimple :: TypeCheckerEnv -> Type -> Simple -> TypeCheckerResult (Type, TypeCheckerEnv)
checkSimple env ty term = do
  (actualTy, env) <- synthesizeSimple env term
  if actualTy `eqType` ty then return (actualTy, env) else throwError (TypeMismatch (getLoc term) actualTy ty)

destructiveLookup :: ASTLoc -> TypeCheckerEnv -> String -> TypeCheckerResult (Type, TypeCheckerEnv)
destructiveLookup loc (env, skEnv) name = case lookup name env of
  Just ty -> return (ty, (deleteBy (\(name', ty') (name, ty) -> name' == name && (ty' `eqType` ty)) (name, ty) env, skEnv))
  Nothing -> throwError (UnboundName loc name)

synthesizeSimple :: TypeCheckerEnv -> Simple -> TypeCheckerResult (Type, TypeCheckerEnv)
synthesizeSimple env (NumberLiteral _ _) = return (TyCon "Number", env)
synthesizeSimple env (StringLiteral _ _) = return (TyCon "String", env)
synthesizeSimple env (Variable loc var) = destructiveLookup loc env var
synthesizeSimple env (UnitSimple _) = return (Unit, env)

elimBindings :: ASTLoc -> [(String, Type)] -> TypeCheckerEnv -> TypeCheckerResult TypeCheckerEnv
elimBindings loc bindings env = do
  -- trace ("eliminating: " ++ show bindings ++ " in: " ++ show env) (return ())
  foldM (\env (name, ty) -> elimBinding loc name ty env) env bindings

elimBindingsOptional :: ASTLoc -> [(String, Type)] -> TypeCheckerEnv -> TypeCheckerResult TypeCheckerEnv
elimBindingsOptional loc bindings env = do
  -- trace ("eliminating: " ++ show bindings ++ " in: " ++ show env) (return ())
  foldM (\env (name, ty) -> elimBindingOptional loc name ty env) env bindings

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition test vals = (filter test vals, filter (not . test) vals)

elimBindingOptional :: ASTLoc -> String -> Type -> TypeCheckerEnv -> TypeCheckerResult TypeCheckerEnv
elimBindingOptional loc name _ ([], _) = do
  -- trace ("eliminating: " ++ name ++ " in empty env") (return ())
  throwError (LinearVarNotUsed loc name undefined)
elimBindingOptional loc bindName bindType (env, skEnv) = do
  -- trace ("eliminating: " ++ bindName) (return ())
  let (matchingBindings, nonMatchingBindings) = partition (\(name, ty) -> bindName == name && bindType `eqType` ty) env
  when (length matchingBindings > 2) (throwError (LinearVarReused loc bindName))
  -- trace ("eliminated: " ++ bindName) (return ())
  return (nonMatchingBindings, skEnv)

elimBinding :: HasCallStack => ASTLoc -> String -> Type -> TypeCheckerEnv -> TypeCheckerResult TypeCheckerEnv
elimBinding loc name _ ([], _) = do
  -- trace ("eliminating: " ++ name ++ " in empty env") (return ())
  throwError (LinearVarNotUsed loc name undefined)
elimBinding loc bindName bindType (env, skEnv) = do
  -- trace ("eliminating: " ++ bindName) (return ())
  let (matchingBindings, nonMatchingBindings) = partition (\(name, ty) -> bindName == name && bindType `eqType` ty) env
  when (null matchingBindings) (throwError (LinearVarNotUsed loc bindName undefined))
  when (length matchingBindings > 2) (throwError (LinearVarReused loc bindName))
  -- trace ("eliminated: " ++ bindName) (return ())
  return (nonMatchingBindings, skEnv)

-- when (name /= bindName) (throwError (LinearVarNotUsed bindName body))
-- when (bindName `elem` map fst remainingEnv) (throwError (LinearVarReused bindName body))
-- when (valType /= bindType) (throwError (TypeMismatch valType bindType))
-- return remainingEnv

-- subtractEnvs :: TypeCheckerEnv -> TypeCheckerEnv -> TypeCheckerResult TypeCheckerEnv
-- subtractEnvs env [] = return env
-- subtractEnvs [] ((varName,_):_) = throwError (UnboundName varName)
-- subtractEnvs ((name1,ty1):env1) ((name2,ty2):env2) | name1 /=

synthesizeRands :: TypeCheckerEnv -> [Simple] -> TypeCheckerResult ([Type], TypeCheckerEnv)
synthesizeRands env [] = return ([], env)
synthesizeRands env (rand : rands) = do
  (randType, randEnv) <- synthesizeSimple env rand
  (randTypes, randsEnv) <- synthesizeRands randEnv rands
  return (randType : randTypes, randsEnv)

checkParam :: ASTLoc -> (Type, Type) -> TypeCheckerResult ()
checkParam loc (t1, t2) = unless (t1 `eqType` t2) (throwError (TypeMismatch loc t1 t2))

checkUsed :: ASTLoc -> String -> TypeCheckerEnv -> TypeCheckerResult ()
checkUsed loc name checkerEnv@(env, _) = when (name `elem` map fst env) (throwError (LinearVarNotUsed loc name checkerEnv))

tupleConstructorHelper :: String -> Maybe Int
tupleConstructorHelper ('T' : 'u' : 'p' : 'l' : 'e' : ':' : s) = Just $ read s
tupleConstructorHelper _ = Nothing

-- pattern Head :: a -> [a]
-- pattern Head x <- x:xs

pattern TupleConstructor :: Int -> String
pattern TupleConstructor n <- (tupleConstructorHelper -> Just n)

makeTupleConstructor :: Int -> TypeCheckerResult Type
makeTupleConstructor n = do
  tyNameStrs <- replicateM n genTypeName
  let varNames = map FreshVarName tyNameStrs
  let tyNames = map TyVar varNames
  return (TySchema [FreshVarName "_secret_:a", FreshVarName "_secret_:b"] (TyFrac (AbstractVar $ FreshVarName "_secret_:a") (VarAddress $ FreshVarName "_secret_:b") (TySchema varNames (TyArrow (CaptureEnv []) tyNames (tuple tyNames)))))

lookupConstructor :: ASTLoc -> TypeCheckerEnv -> String -> TypeCheckerResult Type
-- lookupConstructor env constructorName | constructorName `startsWith` "Tuple:" = undefined
lookupConstructor loc _ (TupleConstructor n) = return $ tupleConstructor n
lookupConstructor loc (env, _) constructorName = case lookup constructorName env of
  Just ty@(TySchema _ (TyFrac _ _ (TyArrow (CaptureEnv []) _ _))) -> return ty
  Just ty -> error $ "not a constructor: " ++ pretty ty--throwError (NotAConstructor constructorName ty)
  Nothing -> throwError (UnknownConstructor loc constructorName)

-- unifyApplication :: Type -> [Term] -> Type
-- unifyApplication (TyArrow env params r) = do
--   foldM

inferParams :: TypeCheckerEnv -> [Simple] -> TypeCheckerResult ([Type], TypeCheckerEnv)
inferParams env =
  foldM
    ( \(tys, env) term -> do
        (ty, env) <- synthesizeSimple env term
        return (ty : tys, env)
    )
    ([], env)

genQNamesForFunc :: [VarName] -> [Type] -> [VarName]
genQNamesForFunc = foldr (map . flip genQName)
-- functionFromArgs paramTypes = let qNames = genQNamesForFunc ["a", "b", "c", "d"] paramTypes

functionFromArgs :: [Type] -> Type
functionFromArgs paramTypes = let qNames = genQNamesForFunc (map FreshVarName ["a", "b", "c", "d"]) paramTypes
                                  a:b:c:[d] = qNames in
                              TySchema qNames (TyFrac (AbstractVar c) (VarAddress d) (TyArrow (CaptureEnvVar a) paramTypes (TyVar b)))

functionFromReturnType :: Int -> Type -> TypeCheckerResult Type
functionFromReturnType arity returnTy = do
  tyNameStrs <- replicateM arity genTypeName <&> map FreshVarName
  let tyNames = map TyVar tyNameStrs
  let a = FreshVarName "a"
  let b = FreshVarName "b"
  let c = FreshVarName "c"
  return (TySchema ([a, b, c] ++ tyNameStrs) (TyFrac (AbstractVar a) (VarAddress b) (TyArrow (CaptureEnvVar c) tyNames returnTy)))

liftUnificationResult :: ASTLoc -> OverallUnificationResult a -> TypeCheckerResult a
liftUnificationResult loc x = liftUnificationResultHelper loc x

liftUnificationResultHelper :: ASTLoc -> Either (Type, Type, UnificationError.UnificationError) a -> TypeCheckerResult a
liftUnificationResultHelper loc (Right val) = return val
liftUnificationResultHelper loc (Left (ty1, ty2, UnificationError.Circularity)) = throwError $ CircularUnification loc
liftUnificationResultHelper loc (Left (ty1, ty2, UnificationError.TypeMismatch innerTy1 innerTy2)) = throwError $ UnificationError loc ty1 ty2 innerTy1 innerTy2
liftUnificationResultHelper loc (Left (ty1, ty2, UnificationError.AddressMismatch addr1 addr2)) = throwError $ AddressMismatch loc addr1 addr2
-- liftUnificationResultHelper loc (Left (UnificationError.FracMismatch frac1 frac2)) = throwError $ FracMismatch loc frac1 frac2

extendEnv :: TypeCheckerEnv -> [(String, Type)] -> TypeCheckerEnv
extendEnv (env, skEnv) bindings = (bindings ++ env, skEnv)

extendEnv1 :: TypeCheckerEnv -> (String, Type) -> TypeCheckerEnv
extendEnv1 (env, skEnv) binding = (binding : env, skEnv)

synthesizeBranch :: TypeCheckerEnv -> Type -> Branch -> TypeCheckerResult (Type, TypeCheckerEnv)
synthesizeBranch env targetType (Branch loc (WildCardPattern _ name) body) = synthesize (extendEnv1 env (name, targetType)) body
-- synthesizeBranch env targetType (ConstructorPattern (tupleConstructor -> n) valNames, body) = do
synthesizeBranch env targetType (Branch loc (ConstructorPattern patternLoc name valNames) body) = do
  -- traceM ("synthesizing branch for constructor " ++ name)
  constructorType <- lookupConstructor patternLoc env name
  ty <- functionFromReturnType (length valNames) targetType
  -- traceM ("unifying " ++ pretty constructorType ++ " with " ++ pretty ty)
  (constructorType, _) <- liftUnificationResult patternLoc (unify constructorType ty)
  synthesize (extendEnv env (zip valNames (constructorFieldTypes constructorType))) body
  -- case constructorType of
  --   (TySchema _ (TyFrac _ _ (TyArrow (CaptureEnv []) valTypes _))) -> do
  --     let nullBindings = filter (eqType Unit . snd) (fst (extendEnv env (zip valNames valTypes)))
  --     (ty, env) <- synthesize (extendEnv env (zip valNames valTypes)) body
  --     env <- elimBindingsOptional nullBindings env
  --     return (ty, env)
  --   notCons -> error $ "not a constructor: " ++ pretty notCons

allSameBy :: (a -> a -> Bool) -> [a] -> Bool
allSameBy _ [] = True
allSameBy _ [_] = True
allSameBy pred (first : rest) = all (pred first) rest

allSame :: (Eq a) => [a] -> Bool
allSame = allSameBy (==)

sameEnv :: (Eq a, Eq b) => [(a, b)] -> [(a, b)] -> Bool
sameEnv [] [] = True
sameEnv _ [] = False
sameEnv [] _ = False
sameEnv (x : xs) ys
  | x `elem` ys = sameEnv xs (x `delete` ys)
  | otherwise = False

allSameEnv :: [TypeCheckerEnv] -> Bool
allSameEnv [] = True
allSameEnv [_] = True
allSameEnv (first : rest) = all (sameEnv (fst first) . fst) rest

checkBranchEnvs :: ASTLoc -> Term -> [(ASTLoc, TypeCheckerEnv)] -> TypeCheckerResult ()
checkBranchEnvs loc term bodies = case convertCaseBodyMismatch bodies of
  Right () -> return ()
  Left err -> throwError $ CaseBodyEnvMismatch loc term err

handleDeref :: Type -> AbstractFrac -> Type
handleDeref (TyFrac frac address ty) derefdFrac = TyFrac (frac |*| derefdFrac) address ty
handleDeref ty _ = ty

mangledTypeName :: String
mangledTypeName = "gen:"

genTypeName :: (Monad m) => TypeCheckerStateT m String
genTypeName = do
  typeNum <- get
  put (typeNum + 1)
  return (mangledTypeName ++ show typeNum)

genAddress :: (Monad m) => TypeCheckerStateT m AbstractAddress
genAddress = do
  num <- get
  put (num + 1)
  return (FixedAddress num)

-- instnatiateCaptureEnv

-- instantiateParameterType :: (Monad m) => Type -> TypeCheckerStateT m Type
-- instantiateParameterType (TyVar _) = do
--   ty <- genTypeName
--   return (TyCon ty)
-- instantiateParameterType ty = return ty

instantiateSchemaType :: (Monad m) => TypeCheckerEnv -> Type -> TypeCheckerStateT m (Type, TypeCheckerEnv)
instantiateSchemaType (tyEnv, state) ty = let (ty', skEnv) = runState (instantiate ty) state in return (ty', (tyEnv, skEnv))

-- instantiateSchemaType renames (TyVar name) | name `elem` map fst renames = return (fromJust (lookup name renames))
-- instantiateSchemaType renames (TySchema name) | name `elem` map fst renames = return (fromJust (lookup name renames))
-- instantiateSchemaType renames ty = return ty

-- getQNames :: Type -> [String]
-- getQNames (TyVar a) = [a]
-- getQNames (TyFrac (AbstractVar a) (VarAddress b) ty) = a : b : getQNames ty
-- getQNames (TyFrac (AbstractVar a) _ ty) = a : getQNames ty
-- getQNames (TyFrac _ (VarAddress a) ty) = a : getQNames ty
-- -- getQNames (_:tys) = getQNames tys
-- getQNames _ = []

-- applyHelper :: TypeCheckerEnv -> Type -> Type -> [Type] -> TypeCheckerResult (Type, TypeCheckerEnv)
-- applyHelper env (TyFrac _ _ (TyArrow _ paramTypes returnType)) ratorType randTypes = do
--       mapM_ checkParam (zip paramTypes randTypes)
--       return (tuple [returnType, ratorType], env)
dropHelper :: TypeCheckerEnv -> Simple -> Type -> TypeCheckerResult (Type, TypeCheckerEnv)
dropHelper env term (TySchema qName ty) = do
  (ty, env) <- dropHelper env term ty
  return (TySchema qName ty, env)
dropHelper env _ (TyFrac _ _ (TyArrow (CaptureEnv []) _ _)) = return (Unit, env)
dropHelper _ term (TyFrac _ _ (TyArrow {})) = throwError (DropClosure (getLoc term) term)
dropHelper env term (TyFrac frac _ (CooperativeRef ty)) = do
  unless (frac == AbstractNum 0 || frac == AbstractNum 1) (throwError (InvalidDrop (getLoc term) term frac))
  return (ty, env)
dropHelper env term (TyFrac frac _ (Ref ty)) = do
  unless (frac == AbstractNum 0 || frac == AbstractNum 1) (throwError (InvalidDrop (getLoc term) term frac))
  return (ty, env)
dropHelper env term (TyFrac frac _ _) = do
  unless (frac == AbstractNum 0 || frac == AbstractNum 1) (throwError (InvalidDrop (getLoc term) term frac))
  return (Unit, env)
dropHelper env _ Unit = return (Unit, env)
dropHelper env _ (TyCon _) = return (Unit, env)
dropHelper _ term ty = throwError (InvalidDropType (getLoc term) ty)

-- instantiateUniqueAddresses :: Type -> TypeCheckerResult Type
-- instantiateUniqueAddresses (TyFrac frac (UniqueAddress _) ty) = do
--   addr <- genAddress
--   return $ TyFrac frac addr ty
-- instantiateUniqueAddresses (TyApp rator rands) = do
--   rands <- mapM instantiateUniqueAddresses rands
--   return (TyApp rator rands)
-- instantiateUniqueAddresses (TySchema qNames ty) = do
--   ty <- instantiateUniqueAddresses ty
--   return $ TySchema qNames ty
-- instantiateUniqueAddresses ty = return ty

-- fracTuple :: [Type] -> TypeCheckerResult Type
-- fracTuple ty = do
--   addr <- genAddress
--   return $ TyFrac (AbstractNum 1) addr (tuple ty)

{--
Non-special cased version
--}

fracTuple :: [Type] -> Type
fracTuple ty = let
                  tupleVal = tuple ty
                  addr = genQName (FreshVarName "addr") tupleVal in
                  TyExists [addr] (TyFrac (AbstractNum 1) (VarAddress addr) tupleVal)

-- reintroduceRator :: TypeCheckerEnv -> Simple -> Type -> TypeCheckerEnv
-- reintroduceRator env (Variable rator) ratorTy = (rator, ratorTy):env

synthesize :: TypeCheckerEnv -> Term -> TypeCheckerResult (Type, TypeCheckerEnv)
synthesize env (Literal pos simple) = synthesizeSimple env simple
synthesize env (Annotation pos ty term) = check env ty term
synthesize env (Application pos (Variable varPos (TupleConstructor n)) rands) = do
  let ratorType = tupleConstructor n
  (randTypes, randsEnv) <- synthesizeRands env rands
  (res, _) <- liftUnificationResult pos (unify ratorType (functionFromArgs randTypes))
  let returnTy = returnType res
  return (returnTy, randsEnv)
synthesize env (Application pos rator rands) = do
  (ratorType, ratorEnv) <- synthesizeSimple env rator
  (randTypes, randsEnv) <- synthesizeRands ratorEnv rands
  traceM (fancyPrintLoc pos)
  -- traceM ("rator: " ++ show ratorType)
  -- traceM ("rands: " ++ show (functionFromArgs randTypes))
  (res, _) <- liftUnificationResult pos (unify ratorType (functionFromArgs randTypes))
  -- let env = reintroduceRator randsEnv rator ratorType
  let returnTy = returnTypeHelper ratorType (returnType res)
  -- traceM $ "rator type is: " ++ pretty ratorType
  return (returnTy, randsEnv)
  where
    returnTypeHelper :: Type -> Type -> Type
    -- returnTypeHelper ratorType (TyFrac frac addr (Tuple _ vs)) = TyFrac frac addr (tuple $ ratorType : vs)
    -- returnTypeHelper ratorType (TySchema qNames (TyFrac frac addr (Tuple _ vs))) = TySchema qNames (TyFrac frac addr (tuple $ ratorType : vs))
    -- returnTypeHelper ratorType (TyExists qNames (TyFrac frac addr (Tuple _ vs))) = TyExists qNames (TyFrac frac addr (tuple $ ratorType : vs))
    returnTypeHelper ratorType ty = fracTuple [ratorType, ty]
-- case ratorType of
--   (TyFrac _ _ (TyArrow _ paramTypes returnType)) -> do
--     mapM_ checkParam (zip paramTypes randTypes)
--     return (tuple [returnType, ratorType], randsEnv)
--   (TyArrow _ paramTypes returnType) -> do
--     mapM_ checkParam (zip paramTypes randTypes)
--     return (tuple [returnType, ratorType], randsEnv)
--   notAFunc -> throwError (NotAFunction notAFunc rator)
synthesize env (LetFunction pos name captures params rType funcBody body) = do
  -- trace (show (map snd params)) (return ())
  let qNames = (params >>= getQuantifiedVars . snd) `union` getQuantifiedVars rType
  -- instantiatedParams <- mapM instantiateParameterType params
  -- _ <- (trace $ "capturing: " ++ show captures) (return ())
  (env, captureTypes) <-
    foldM
      ( \(env, captureTypes) captureName -> do
          (ty, env) <- destructiveLookup pos env captureName
          return (env, ty : captureTypes)
      )
      (env, [])
      captures
  -- let paramTypes = map snd params
  addr <- genAddress
  let funcType = if null qNames
                  then TyFrac (AbstractNum 1) addr (TyArrow (CaptureEnv captureTypes) (map snd params) rType)
                  else TySchema qNames (TyFrac (AbstractNum 1) addr (TyArrow (CaptureEnv captureTypes) (map snd params) rType))
  (instanceTy, env) <- instantiateSchemaType env funcType
  -- _ <- trace (pretty funcType) (return ())
  -- _ <- trace (pretty instanceTy) (return ())
  -- traceM ("param types: " ++ show (map pretty (paramTypes instanceTy)))
  -- traceM ("return type: " ++ pretty (returnType instanceTy))
  -- traceM ("letfun type: " ++ show instanceTy)
  (_, env) <- check (extendEnv env (zip (map fst params) (paramTypes instanceTy))) (returnType instanceTy) funcBody
  (bodyType, env) <- synthesize (extendEnv1 env (name, funcType)) body
  checkUsed pos name env
  return (bodyType, env)
synthesize env (LetRecFunction pos name captures params rType funcBody body) = do
  -- params <- mapM instantiateParameterType params
  -- traceM "test"
  let qNames = (params >>= getQuantifiedVars . snd) `union` getQuantifiedVars rType
  (env, captureTypes) <-
    foldM
      ( \(env, captureTypes) captureName -> do
          (ty, env) <- destructiveLookup pos env captureName
          return (env, ty : captureTypes)
      )
      (env, [])
      captures
  -- let paramTypes = map snd params
  addr <- genAddress
  let funcType = TySchema qNames (TyFrac (AbstractNum 1) addr (TyArrow (CaptureEnv captureTypes) (map snd params) rType))
  -- traceM ("letrec raw func type: " ++ pretty funcType)
  -- let funcType = TySchema qNames (TyFrac (AbstractNum 1) addr (TyArrow (CaptureEnv captureTypes) (map snd params) rType))
  (instanceTy, env) <- instantiateSchemaType env funcType
  -- traceM ("letrec type: " ++ pretty instanceTy)
  -- traceM ("params w types: " ++ pretty (zip (map fst params) (paramTypes instanceTy)))
  (_, env) <- check (extendEnv env ((name, funcType) : zip (map fst params) (paramTypes instanceTy))) (returnType instanceTy) funcBody
  -- (_, env) <- check (extendEnv env ((name, funcType):params)) returnType funcBody
  -- checkUsed pos name env
  (bodyType, env) <- synthesize env body
  checkUsed (getLoc body) name env
  -- checkUsed pos name env
  -- env <- elimBinding pos name funcType env
  -- let env' = extendEnv1 env (name, funcType)
  -- checkUsed pos name env
  return (bodyType, env)
synthesize env (UnitExpr pos) = return (Unit, env)
synthesize env (Thunk pos captures body) = do
  (thunkType, env) <- synthesize env body
  (env, captureTypes) <-
    foldM
      ( \(env, captureTypes) captureName -> do
          (ty, env) <- destructiveLookup pos env captureName
          return (env, ty : captureTypes)
      )
      (env, [])
      captures
  return (TyArrow (CaptureEnv captureTypes) [Unit] thunkType, env)
synthesize _ term@(Case pos _ []) = throwError (EmptyCase pos term)
synthesize env term@(Case pos target branches) = do
  (targetType, env) <- synthesizeSimple env target
  -- trace (show targetType) (return ())
  branchResults <- mapM (synthesizeBranch env targetType) branches
  let branchTypes = map fst branchResults
  let branchEnvs = map snd branchResults
  unless (allSameBy eqType branchTypes) (throwError (CaseBodyTypeMismatch pos term branchTypes))
  -- trace (show branchEnvs) (return ())
  -- unless (allSameEnv branchEnvs) (throwError (CaseBodyEnvMismatch pos term (zip (map getBranchLoc branches) branchEnvs)))
  checkBranchEnvs pos term (zip (map getBranchLoc branches) branchEnvs)
  return (head branchTypes, head branchEnvs)
  where getBranchLoc :: Branch -> ASTLoc
        getBranchLoc (Branch loc _ _) = loc
synthesize env (LetPattern pos (WildCardPattern _ name) val body) = do
  (valType, env) <- synthesize env val
  synthesize (extendEnv1 env (name, valType)) body
synthesize env (LetPattern pos (ConstructorPattern patternLoc constructorName valNames) val body) = do
  (valType, env) <- synthesize env val
  constructorType <- lookupConstructor patternLoc env constructorName
  gennedFun <- functionFromReturnType (arity constructorType) valType
  -- trace ("check") (return ())
  -- trace (show valType) (return ())
  -- trace (show constructorType) (return ())
  -- traceM ("constructor: " ++ pretty constructorType ++ " genned fun: " ++ pretty gennedFun)
  (constructorType, _) <- liftUnificationResult patternLoc (unify constructorType gennedFun)

  synthesize (extendEnv env (zip valNames (constructorFieldTypes constructorType))) body
  -- let pparamTypes
  -- trace (show constructorType) (return ())
  -- case constructorType of
  --   (TyFrac _ _ (TyArrow (CaptureEnv []) valTypes _)) -> synthesize (extendEnv env (zip valNames valTypes)) body
  --   (TySchema _ (TyFrac _ _ (TyArrow (CaptureEnv []) valTypes _))) -> synthesize (extendEnv env (zip valNames valTypes)) body
  --   notCons -> error $ "not a constructor (in let): " ++ pretty notCons
{--

∀ a0, b0.Frac<[(["a0"],1 % 1)], VarAddress "b0",
  ∀ a, b.Frac<[(["a"],1 % 1)], VarAddress "b",
  Number -> Number -> Number> ->
    ∀ c0, d0.Number -> (Tuple:2 ∀ a, b.Frac<[(["a"],1 % 1)], VarAddress "b", Number -> Number -> Number> ∀ c0, d0.Number)>

--}
-- when (valType /= constructorType) (throwError (CaseConstructorMismatch valType constructorType))
synthesize env (Move pos term) = error "Move isn't needed! (I think)" -- do
-- (valType, env) <- synthesizeSimple env term
-- case valType of
--   (TySchema qNames (TyFrac frac address ty)) -> return (TySchema qNames (TyFrac frac address ty), env)
--   (TyFrac frac address ty) -> return (TyFrac frac address ty, env)
--   notRef -> throwError (NotARef notRef term)
synthesize env (Split pos term) = do
  (valType, env) <- synthesizeSimple env term
  case valType of
    (TySchema qNames (TyFrac frac address ty)) -> do
      let tupleTy = fracTuple [TySchema qNames (TyFrac (frac |/| 2) address ty), TySchema qNames (TyFrac (frac |/| 2) address ty)]
      return (tupleTy, env)
    (TyFrac frac address ty) -> do
      let tupleTy = fracTuple [TyFrac (frac |/| 2) address ty, TyFrac (frac |/| 2) address ty]
      return (tupleTy, env)
    notRef -> throwError (NotARef pos notRef term)
synthesize env (RefExpr pos expr) = do
  (ty, env) <- synthesizeSimple env expr
  addr <- genAddress
  return (TyFrac (AbstractNum 1) addr (Ref ty), env)
synthesize env (Drop pos term) = do
  (valType, env) <- synthesizeSimple env term
  dropHelper env term valType
synthesize env (Semicolon pos e1 e2) = do
  (_, env) <- check env Unit e1
  synthesize env e2
synthesize env (SetRef pos eRef eVal) = do
  (refType, env) <- synthesizeSimple env eRef
  case refType of
    (TyFrac (AbstractFrac (Just 1) []) _ (TyApp (TyCon "Ref") [ty])) -> do
      (_, env) <- checkSimple env ty eVal
      return (ty, env)
    (TyFrac frac _ _) -> throwError (SetPartialRef pos eRef frac)
    notRef -> throwError (NotARef pos notRef eRef)
synthesize env (DerefIn pos name eRef body) = do
  (refType, env) <- synthesizeSimple env eRef
  case refType of
    (TyFrac (AbstractFrac (Just 0) []) _ _) -> throwError (DeRefEmptyRef pos eRef)
    (TyFrac frac _ ty) -> do
      let refVal = handleDeref ty frac
      (bodyTy, env) <- synthesize (extendEnv1 env (name, refVal)) body
      case lookup name (fst env) of
        Just endingTy -> unless (ty == endingTy) (throwError (DerefInConsumed pos))
        Nothing -> throwError (DerefInConsumed pos)
      return (bodyTy, env)
    notRef -> throwError (NotARef pos notRef eRef)
synthesize _ term@(List pos []) = throwError (InferenceError pos term)
synthesize env (List pos (e : es)) = do
  (eTy, env) <- synthesizeSimple env e
  env <- foldM (\env e -> checkSimple env eTy e <&> snd) env es
  return (TyApp (TyCon "List") [eTy], env)
synthesize env (DataTypeDeclaration pos tyName qNamesRaw constructors body) = do
  let qNames = map FreshVarName qNamesRaw
  let constructorReturnTypeRaw = TyApp (TyCon tyName) (map TyVar qNames)
  let returnAddr = genQName (FreshVarName "addr") constructorReturnTypeRaw
  let returnType = TyExists [returnAddr] (TyFrac (AbstractNum 1) (VarAddress returnAddr) constructorReturnTypeRaw)
  constructorAddr <- genAddress
  let constructorTypes =
        map
          ( \(name, params) ->
              let paramTys =
                    if null params
                      then [Unit]
                      else params
                  constructorTy = TyArrow (CaptureEnv []) paramTys returnType
               in (name, TySchema qNames (TyFrac (AbstractNum 1) constructorAddr constructorTy))
          )
          constructors
  -- traceM (pretty constructorTypes)
  (ty, env) <- synthesize (extendEnv env constructorTypes) body
  -- trace ("b4" ++ show env) (return ())
  env <- foldM (\env (name, ty) -> elimBindingOptional pos name ty env) env constructorTypes
  -- trace ("after: " ++ show env) (return ())
  return (ty, env)
synthesize env (Parallel pos branches) = do
  env <- foldM (\env branch -> check env Unit branch <&> snd) env branches
  return (Unit, env)
synthesize env (Cooperate pos eRef) = do
  (refType, env) <- synthesizeSimple env eRef
  case refType of
    (TyFrac frac@(AbstractFrac (Just 1) []) address (TyApp (TyCon "Ref") ty)) -> return (TyFrac frac address (TyApp CooperativeRefType ty), env)
    (TyFrac frac _ _) -> throwError (CooperatePartialRef pos eRef frac)
    notRef -> throwError (NotARef pos notRef eRef)
synthesize env (Share pos eRef) = do
  (refType, env) <- synthesizeSimple env eRef
  case refType of
    (TyFrac frac@(AbstractFrac (Just 1) []) address (TyApp (TyCon "CooperativeRef") ty)) -> return (TyFrac frac address (TyApp refType ty), env)
    (TyFrac frac _ _) -> throwError (SharePartialRef pos eRef frac)
    notRef -> throwError (NotARef pos notRef eRef)
synthesize env (UnClos pos eClos) = do
  (clos, env) <- synthesizeSimple env eClos
  unClosHelper env clos
  where
    unClosHelper :: TypeCheckerEnv -> Type -> TypeCheckerResult (Type, TypeCheckerEnv)
    unClosHelper env (TySchema _ ty) = unClosHelper env ty
    unClosHelper env (TyFrac (AbstractFrac (Just 1) []) _ (TyArrow closEnv@(CaptureEnv captures) _ _)) = return (tuple captures, env)
    unClosHelper env (TyFrac _ _ (TyArrow closEnv@(CaptureEnvVar _) _ _)) = throwError (UnClosPolyEnv pos eClos closEnv)
    -- unClosHelper env (TyFrac _ _ (TyArrow closEnv@(CaptureEnvVar _) _ _)) = throwError (UnClosPartial pos eClos)
    unClosHelper env notRef = throwError (NotAClosure pos notRef eClos)
synthesize env (Acquire pos ref name body) = do
  (refType, env) <- synthesizeSimple env ref
  case refType of
    (TyFrac _ _ (TyApp (TyCon "CooperativeRef") [ty])) -> do
      (bodyTy, env) <- synthesize (extendEnv1 env (name, ty)) body
      checkUsed pos name env
      return (tuple [bodyTy, refType], env)
    notRef -> throwError (NotARef pos notRef ref)
synthesize env (Merge pos e1 e2) = do
  (ty1, env) <- synthesizeSimple env e1
  (ty2, env) <- synthesizeSimple env e2
  case (ty1, ty2) of
    (TyFrac frac1 addr1 ty1, TyFrac frac2 addr2 ty2) -> do
      unless (addr1 == addr2) (throwError (AddressMismatch pos addr1 addr2))
      unless (ty1 == ty2) (throwError (TypeMismatch pos ty1 ty2))
      return (TyFrac (frac1 |+| frac2) addr1 ty1, env)
    (_, notRef) -> throwError (NotAClosure pos notRef e1)
synthesize env (Rebalance pos []) = throwError (EmptyRebalance pos)
synthesize env (Rebalance pos [e]) = synthesizeSimple env e
synthesize env (Abstract pos e) = do
  (refTy, env) <- synthesizeSimple env e
  case refTy of
    -- TyExists [qName] (TyFrac frac (VarAddress addrVar) ty) | addrVar == qName -> return (TyFrac frac addr ty, env)
    (TyFrac frac _ ty) -> let addr = genQName (FreshVarName "a") ty in return (TyExists [addr] (TyFrac frac (VarAddress addr) ty), env)
    notFracTy -> throwError $ InvalidAbstract pos notFracTy
synthesize env (Unabstract pos e) = do
  (refTy, env) <- synthesizeSimple env e
  addr <- genAddress
  case refTy of
    TyExists [qName] (TyFrac frac (VarAddress addrVar) ty) | addrVar == qName -> return (TyFrac frac addr ty, env)
    -- other -> error $ "other: " ++ show other
synthesize env (Rebalance pos (e : es)) = do
  (ty, env) <- synthesizeSimple env e
  case ty of
    (TyFrac frac addr cellTy) -> do
      (env, newFrac) <- rebalanceHelper addr env es
      let balancedFrac = newFrac |/| fromIntegral (length (e : es))
      return (tuple (replicate (length (e : es)) (TyFrac balancedFrac addr cellTy)), env)
  where
    rebalanceHelper :: AbstractAddress -> TypeCheckerEnv -> [Simple] -> TypeCheckerResult (TypeCheckerEnv, AbstractFrac)
    rebalanceHelper _ env [] = return (env, AbstractNum 0)
    rebalanceHelper addr env (e : es) = do
      (ty, env) <- synthesizeSimple env e
      case ty of
        (TyFrac frac eAddr _)
          | eAddr /= addr -> throwError (AddressMismatch pos addr eAddr)
          | otherwise -> rebalanceHelper addr env es >>= \(env, newFrac) -> return (env, frac |+| newFrac)

mergeHelper :: TypeCheckerEnv -> Type -> Simple -> TypeCheckerResult (Type, TypeCheckerEnv)
mergeHelper env (TyFrac frac1 addr refTy) e = do
  let a = FreshVarName "a"
  (fracTy, env) <- checkSimple env (TySchema [a] (TyFrac (AbstractVar a) addr refTy)) e
  case fracTy of
    (TyFrac frac2 _ _) -> return (TyFrac (frac1 |+| frac2) addr refTy, env)
    _ -> error "This should be unreachable"
mergeHelper env ty e = checkSimple env ty e

-- (tys, env) <- foldM (\(rest,env) e -> synthesizeSimple env e >>= \(ty, env) -> return (ty:rest, env)) ([], env) es
-- synthesize env ()

-- convertConstructorParamType :: String -> Type
-- convertConstructorParamType paramTy = if isLower paramTy then TyVar paramTy else TyCon
-- synthesize env (Application rator rands) = do
--   (ratorType, ratorEnv) <- synthesizeSimple rator
--   (randTypes, randsEnv) <- synthesizeRands rands
--   case ratorType of
--     (TyArrow paramTypes returnType) -> do
--       mapM checkParam (zip paramTypes randTypes)
--       return (tuple $ paramTypes ++ [returnType] ++ [ratorType], ratorEnv ++ randsEnv)
-- synthesize env (Annotation ty term) = check ty term
-- synthesize env (Literal term) = synthesizeSimple term
-- synthesize env (Let valName valExpr bodyExpr) = do
--   (valType, valEnv) <- synthesize env valExpr
--   (bodyType, bodyEnv) <- synthesize valEnv bodyExpr
--   remainingBodyEnv <- elimBinding valName valType bodyEnv bodyExpr
--   return (bodyType, valEnv ++ remainingBodyEnv)
-- synthesize env (Move expr) = do
--   (ty, env) <- synthesizeSimple expr
--   case ty of
--     (TyFrac frac address refType) -> return (tuple [TyFrac frac address refType, TyFrac (abstract 0) address refType], env)
--     notRefTy -> throwError (NotARef notRefTy (Literal expr))
-- synthesize env (Split expr) = do
--   (ty, env) <- synthesizeSimple expr
--   case ty of
--     (TyFrac frac address refType) ->
--       return (tuple [TyFrac (frac |/| 2) address refType, TyFrac (frac |/| 2) address refType], env)
--     notRefTy -> throwError (NotARef notRefTy (Literal expr))
-- synthesize env (LetFunction name params returnType funcBody body) = do
--   let paramTypes = map snd params
--   let funcType = TyArrow paramTypes returnType
--   (_, funcBodyEnv) <- check returnType funcBody
--   elimBindings params funcBodyEnv funcBody
--   (bodyType, bodyEnv) <- synthesize body
--   remainingBodyEnv <- elimBinding name funcType bodyEnv body
--   return (bodyType, funcBodyEnv ++ remainingBodyEnv)
-- synthesize env (LetRecFunction name params returnType funcBody body) = do
--   let paramTypes = map snd params
--   let funcType = TyArrow paramTypes returnType
--   (_, funcBodyEnv) <- check returnType funcBody
--   remainingFuncBodyEnv <- elimBinding name funcType funcBodyEnv body
--   elimBindings params funcBodyEnv funcBody
--   (bodyType, bodyEnv) <- synthesize body
--   remainingBodyEnv <- elimBinding name funcType bodyEnv body
--   return (bodyType, remainingFuncBodyEnv ++ remainingBodyEnv)
-- synthesize env Unit = return (unit, [])
-- synthesize env (Thunk body) = do
--   (ty, bodyEnv) <- synthesize body
--   return (TyArrow [unit] ty, bodyEnv)
-- synthesize

-- synthesize (LetFunction)
-- synthesize (Merge expr1 expr2) = do
--   (ty1, env1) <- synthesizeSimple expr1
--   (ty2, env2) <- synthesizeSimple expr2
--   case (ty1, ty2) of
--     (TyFrac frac1 address1 refType1, TyFrac frac2 address2 refType2) ->
--       return (tupleType [TyFrac (DivExpr frac (AbstractLiteral 2)) refType, TyFrac (DivExpr frac (AbstractLiteral 2)) refType], env)
--     notRefTy -> throwError (NotARef notRefTy (Literal expr))

{--

let x = 5
let x = 6
x + x

let x = 5
let y = x
let x = 6
x + y
--}

check :: TypeCheckerEnv -> Type -> Term -> TypeCheckerResult (Type, TypeCheckerEnv)
check env (TyApp (TyCon "List") ty) (List pos []) = return (TyApp (TyCon "List") ty, env)
check env ty term = do
  (actualTy, env) <- synthesize env term
  if actualTy `eqType` ty then return (actualTy, env) else throwError (TypeMismatch (getLoc term) actualTy ty)

basicFunc :: [Type] -> Type -> Type
basicFunc paramTys returnTy = funcHelper (TyArrow (CaptureEnv []) paramTys returnTy)

funcHelper :: Type -> Type
funcHelper ty = let a = genQName' "a" ty
                    b = genQName' "b" ty in
                TySchema [a, b] (TyFrac (AbstractVar a) (VarAddress b) ty)

-- basicConstructor

binNumFunc :: Type
binNumFunc = basicFunc [number, number] number

-- cons :: Type
-- cons = TySchema ["a", "b", "c"] (TyFrac (AbstractVar "b") (VarAddress "c") (TyArrow (CaptureEnv []) [TyVar "a", TyApp (TyCon "List") [TyVar "a"]] (TyApp (TyCon "List") [TyVar "a"])))

-- nil :: Type
-- nil = TySchema ["a", "b", "c"] (TyFrac (AbstractVar "b") (VarAddress "c") (TyArrow (CaptureEnv []) [Unit] (TyApp (TyCon "List") [TyVar "a"])))

baseFunctions :: [(String, Type)]
baseFunctions = [("+", binNumFunc), ("-", binNumFunc), ("*", binNumFunc), ("/", binNumFunc)]--, ("Cons", cons), ("Nil", nil)]

wrapExistentialFrac :: Type -> Type
wrapExistentialFrac ty = let addr = genQName' "addr" ty in TyExists [addr] (TyFrac (AbstractNum 1) (VarAddress addr) ty)

simpleFunc :: [Type] -> Type -> Type
simpleFunc paramTys returnTy = TyArrow (CaptureEnv []) paramTys returnTy

numToLetter :: Int -> String
numToLetter n = "a" ++ show n

tupleConstructor :: Int -> Type
tupleConstructor n = let qNames = map (FreshVarName . numToLetter) [1..n]
                         qVars = map TyVar qNames in
                      funcHelper (TySchema qNames (simpleFunc qVars (wrapExistentialFrac (tuple qVars))))

twopleConstructor = tupleConstructor 2

baseConstructors :: [(String, Type)]
baseConstructors = []--[("Nil", funcHelper (TySchema ["a"] (TyArrow (CaptureEnv []) [Unit] (TyExists ["addr"] (TyApp (TyCon "List") [TyVar "a"])))))]

baseTypeBindings :: [(String, Type)]
baseTypeBindings = baseFunctions ++ baseConstructors

baseTypeCheckerEnv :: TypeCheckerEnv
baseTypeCheckerEnv = (baseTypeBindings, [])

elimUnits :: TypeCheckerEnv -> TypeCheckerEnv
elimUnits (tyEnv, skEnv) = (filter (\(_, ty) -> ty /= Unit) tyEnv, skEnv)

checkProgram :: Term -> TypeCheckerResult Type
checkProgram program = do
  (ty, env) <- synthesize baseTypeCheckerEnv program
  -- trace (show env) (return ())
  env <- elimBindings (getLoc program) baseTypeBindings env
  -- trace (show env) (return ())
  if null (fst env) then return ty else throwError (NonEmptyEnv (getLoc program) env)

runTypeChecker :: (Pretty a) => TypeCheckerResult a -> String
runTypeChecker res = case evalStateT res 0 of
  Right ty -> "Type checking succeded with type of: " ++ pretty ty
  Left err -> "Type checking failed with error: " ++ pretty err

-- testProgram = LetFunction "test" [] [("_", Unit)] number (Semicolon (Literal (Variable "_") )(Literal (NumberLiteral 5))) (Literal (Variable "test"))

-- testProgram = LetPattern (WildCardPattern "x") (Literal (NumberLiteral 5)) (Literal (NumberLiteral 5))
-- letTuple :: [String] -> Term -> Term -> Term
-- letTuple names = LetPattern undefined (ConstructorPattern ("Tuple:" ++ show (length names)) names)

-- testProgram = LetPattern (WildCardPattern "x") (RefExpr (NumberLiteral 5)) (Drop (Variable "x"))

-- splitTestProgram = LetPattern (WildCardPattern "x") (RefExpr (NumberLiteral 5)) (letTuple ["a", "b"] (Split (Variable "x")) (Drop (Variable "a")))

-- applicationTestProgram = LetFunction "id" [] [("x", TyVar "a")] (TyVar "a") (Literal (Variable "x")) (UnClos [] (Variable "id"))

-- test = runTypeChecker $ checkProgram applicationTestProgram

checkFile :: String -> IO String
checkFile fileName = do
  handle <- openFile fileName ReadMode
  src <- hGetContents handle
  case parseFile fileName src of
    (Left err) -> error $ "Parser error: " ++ show err
    (Right program) -> return $ runTypeChecker $ checkProgram (transpileProgram (File fileName src) program)

test = checkFile "HelloWorld.hm"

-- test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
-- tests = TestList [TestLabel "test1" test1]