{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# HLINT ignore "Redundant bracket" #-}
-- {-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS -Wall #-} 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}

module Types (substAliasType, genQNameRaw, genQNameRawList, rawTuple, toRaw, RawType(..), unify', genQName, genQName', constructorFieldTypes, paramTypes, skolemize, instantiate, pattern Tuple, eqType, getQuantifiedVars, unify, runUnificationResult, returnType, arity, UnificationError(..), UnificationResult, AbstractId, AbstractExpr (..), Type (..), AbstractFrac (..), AbstractAddress (..), CaptureEnv (..), (|+|), (|/|), (|*|), pattern CooperativeRefType, pattern CooperativeRef, pattern AbstractNum, pattern AbstractVar, tuple, tupleType, pattern Unit, string, number, pattern ListType, pattern RefType, pattern Ref, OverallUnificationResult, VarName(..), pattern FreshVarName, pattern TupleConstructor) where

import Control.Monad (join, unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (MonadState (get, put), State, evalState, StateT (runStateT))
import Data.Either (isRight)
import Data.Functor ((<&>))
import Data.List (intercalate, intersect, sort, union, (\\))
import Debug.Trace (trace, traceM)
import GHC.Stack (HasCallStack, callStack, popCallStack, prettyCallStack)
import Pretty (Pretty (..))
import Control.Monad.Trans.State (runState)
import GHC.Stack.Types (SrcLoc)
import GHC.Stack.Types (getCallStack)

-- import Types (Tuple)

tupleType :: Int -> Type
tupleType n = TyCon ("Tuple:" ++ show n)

-- unit :: Type
-- unit = tupleType 0

pattern Unit :: Type
pattern Unit = Tuple 0 []

tupleConstructor :: String -> Maybe Int
tupleConstructor ('T' : 'u' : 'p' : 'l' : 'e' : ':' : s) = Just $ read s
tupleConstructor _ = Nothing

pattern TupleConstructor :: Int -> String
pattern TupleConstructor n <- (tupleConstructor -> Just n)

tuplePatHelper :: Type -> Maybe (Int, [Type])
tuplePatHelper (TyApp (TyCon (TupleConstructor n)) vals) = Just (n, vals)
tuplePatHelper _ = Nothing

pattern Tuple :: Int -> [Type] -> Type
pattern Tuple n vals <- (tuplePatHelper -> Just (n, vals))
  where
    Tuple arity vals
      | arity == length vals = tuple vals
      | otherwise = error "Arity mismatch in tuple constructor"

string :: Type
string = TyCon "String"

number :: Type
number = TyCon "Number"

-- listType :: Type
pattern ListType :: Type
pattern ListType = TyCon "List"

-- refType :: Type
pattern RefType :: Type
pattern RefType = TyCon "Ref"

-- cooperativeRefType :: Type
pattern CooperativeRefType :: Type
pattern CooperativeRefType = TyCon "CooperativeRef"

-- pattern AnyRefType :: Type -> Type
-- pattern AnyRefType (RefType ty)

pattern Ref :: Type -> Type
pattern Ref ty = TyApp RefType [ty]

pattern CooperativeRef :: Type -> Type
pattern CooperativeRef ty = TyApp CooperativeRefType [ty]

-- eitherRef :: Type -> Maybe Type
-- eitherRef (Ref ty) = Just ty
-- eitherRef (CooperativeRef ty) = Just ty
-- eitherRef _ = Nothing

-- pattern EitherRef :: Type -> Type
-- pattern EitherRef i <- (eitherRef -> Just i)

-- ref :: Type -> Type
-- ref refType = TyApp refType [ty]

-- cooperativeRef :: Type -> Type
-- cooperativeRef ty = TyApp cooperativeRefType [ty]

data AbstractExpr
  = AbstractLiteral Float
  | Var String
  | PlusExpr AbstractExpr AbstractExpr
  | MinusExpr AbstractExpr AbstractExpr
  | TimesExpr AbstractExpr AbstractExpr
  | DivExpr AbstractExpr AbstractExpr
  deriving (Show)

-- data AbstractFracPart
--   = FracLiteral Float
--     | FracVar String Int Int

type AbstractId = VarName

data AbstractFrac = AbstractFrac (Maybe Rational) [([AbstractId], Rational)] | AbstractAny
  deriving (Show)

instance Pretty CaptureEnv where
  pretty :: CaptureEnv -> String
  pretty (InstantiatedCaptureEnvVar v) = "#" ++ pretty v
  pretty (CaptureEnv env) = show env
  pretty (CaptureEnvVar v) = pretty v

instance Pretty AbstractAddress where
  pretty :: AbstractAddress -> String
  pretty (VarAddress s) = pretty s
  pretty (InstantiatedAddress s) = "#" ++ pretty s
  pretty (FixedAddress i) = show i

instance Pretty AbstractFrac where
  pretty :: AbstractFrac -> String
  pretty (AbstractFrac (Just 1) []) = show (1 :: Integer)
  pretty (AbstractFrac (Just n) []) = show n
  pretty (AbstractFrac Nothing [([s], 1)]) = pretty s
  pretty (AbstractFrac Nothing vars) = show vars
  pretty (AbstractFrac fixed vars) = show fixed ++ show vars
  pretty (AbstractAny) = "*"

pattern Owned :: AbstractAddress -> Type -> Type
pattern Owned addr ty = TyFrac (AbstractFrac (Just 1) []) addr  ty

data CaptureEnv = CaptureEnvVar VarName | InstantiatedCaptureEnvVar VarName | CaptureEnv [Type]
  deriving (Show, Eq)

instance Eq AbstractFrac where
  (==) :: AbstractFrac -> AbstractFrac -> Bool
  (==) (AbstractFrac c1 vs1) (AbstractFrac c2 vs2) = c1 == c2 && all (uncurry (==)) (zip (sort vs1) (sort vs2))
  (==) AbstractAny _ = True
  (==) _ AbstractAny = True

assocListCombine :: (Eq k) => (v -> v -> v) -> [(k, v)] -> [(k, v)] -> [(k, v)]
assocListCombine _ l1 [] = l1
assocListCombine f l1 ((k, v) : xs) = (k, maybe v (f v) (lookup k l1)) : assocListCombine f l1 xs

pattern AbstractNum :: Rational -> AbstractFrac
pattern AbstractNum i = AbstractFrac (Just i) []

-- abstract :: Integer -> AbstractFrac
-- abstract i = AbstractFrac (Just (fromInteger i)) []

pattern AbstractVar :: VarName -> AbstractFrac
pattern AbstractVar s = AbstractFrac Nothing [([s], 1)]

(|+|) :: AbstractFrac -> AbstractFrac -> AbstractFrac
(|+|) (AbstractFrac c1 vs1) (AbstractFrac c2 vs2) = AbstractFrac ((+) <$> c1 <*> c2) (assocListCombine (+) vs1 vs2)
(|+|) AbstractAny _ = AbstractAny
(|+|) _ AbstractAny = AbstractAny

-- timesHelper1 :: ([AbstractId], Rational) -> ([AbstractId], Rational) -> ([AbstractId], Rational)
-- timesHelper1 (ids1, _) (ids2, _) | any (`elem` ids2) ids1 = error "Squaring needed in frac multiplication"
-- timesHelper1 (ids1, r1) (ids2, r2) = (ids1 ++ ids2, r1 * r2)

-- timesHelper2 :: Rational -> ([AbstractId], Rational) -> ([AbstractId], Rational)
-- timesHelper2 r2 (ids, r1) = (ids, r1 * r2)

(|*|) :: AbstractFrac -> AbstractFrac -> AbstractFrac
(|*|) (AbstractFrac (Just r1) []) (AbstractFrac (Just r2) []) = AbstractFrac (Just $ r1 * r2) []
(|*|) (AbstractFrac (Just r1) []) (AbstractFrac Nothing [(ids, r2)]) = AbstractFrac Nothing [(ids, r1 * r2)]
(|*|) (AbstractFrac Nothing []) (AbstractFrac _ _) = (AbstractFrac Nothing [])
(|*|) (AbstractFrac Nothing [(ids1, r1)]) (AbstractFrac Nothing [(ids2, r2)])
  | any (`elem` ids2) ids1 = error "Squaring needed in frac multiplication"
  | otherwise = AbstractFrac Nothing [(ids1 ++ ids2, r1 * r2)]
(|*|) (AbstractFrac c vs) f2 =
  foldr
    ((|+|) . (\v -> (AbstractFrac Nothing [v]) |*| f2))
    ((AbstractFrac c []) |*| f2)
    vs
(|*|) AbstractAny _ = AbstractAny
-- (|*|) _ AbstractAny = AbstractAny

(|/|) :: AbstractFrac -> Integer -> AbstractFrac
(|/|) (AbstractFrac c vs) n = AbstractFrac (((/) <$> c) <*> Just (fromInteger n)) (map (\(k, v) -> (k, v / fromInteger n)) vs)
(|/|) AbstractAny _ = AbstractAny

instance Pretty AbstractExpr where
  pretty :: AbstractExpr -> String
  pretty (AbstractLiteral n) = show n
  pretty (Var v) = v
  pretty (PlusExpr x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
  pretty (MinusExpr x y) = "(" ++ pretty x ++ " - " ++ pretty y ++ ")"
  pretty (TimesExpr x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"
  pretty (DivExpr x y) = "(" ++ pretty x ++ " / " ++ pretty y ++ ")"

-- abstractEq :: AbstractExpr -> AbstractExpr -> Bool
-- abstractEq e1 e2 = error "implementation to come"

data AbstractAddress = FixedAddress Int | VarAddress VarName | InstantiatedAddress VarName
  deriving (Eq, Show)

data VarName = VarName String Int deriving (Show, Eq, Ord)

pattern FreshVarName :: String -> VarName
pattern FreshVarName s = VarName s 0

inc :: VarName -> VarName
inc (VarName s i) = VarName s (i + 1)

-- freshTyVar = TyVar 

instance Pretty VarName where
  pretty (VarName s i) = s ++ "[" ++ show i ++ "]"

data RawType
  = RawTyCon String
  | RawTyVar VarName
  | RawTyArrow CaptureEnv [RawType] RawType
  | RawTyApp RawType [RawType]
  | RawTyFrac AbstractFrac AbstractAddress RawType
  | RawTySchema [VarName] RawType
  | RawTyExists [VarName] RawType
  deriving (Show, Eq)

instance Pretty RawType where
  pretty = show

substAliasType :: [(VarName, Type)] -> Type -> Type
substAliasType substs ty = applySubsts (substs, [], [], []) ty

toRaw :: Type -> RawType
toRaw (TyCon s) = RawTyCon s
toRaw (TyVar v) = RawTyVar v
toRaw (TyArrow env params retType) = RawTyArrow env (map toRaw params) (toRaw retType)
toRaw (TyFrac frac addr ty) = RawTyFrac frac addr (toRaw ty)
toRaw (TyApp ratorTy randTys) = RawTyApp (toRaw ratorTy) (map toRaw randTys)
toRaw (TySchema qNames ty) = RawTySchema qNames (toRaw ty)
toRaw (TyExists qNames ty) = RawTyExists qNames (toRaw ty)
toRaw ty@(TyAlias {}) = error $ "Cannot convert aliased type: " ++ pretty ty ++ " to a raw type"
-- data RawType = RawType Type

data Type
  = TyCon String
  | TyArrow CaptureEnv [Type] Type
  | TyApp Type [Type]
  | TyFrac AbstractFrac AbstractAddress Type
  | TyVar VarName
  | TySchema [VarName] Type
  | TyExists [VarName] Type
  | TyAlias RawType Type
  deriving (Show, Eq)

instance Pretty Type where
  pretty :: Type -> String
  pretty Unit = "()"
  pretty (TyCon s) = "#" ++ s
  pretty (TyArrow (CaptureEnv []) args ret) = intercalate " -> " (map pretty args) ++ " -> " ++ pretty ret
  pretty (TyArrow captures args ret) = pretty captures ++ " | " ++ intercalate " -> " (map pretty args) ++ " -> " ++ pretty ret
  pretty (TyApp con []) = "(" ++ pretty con ++ " #empty" ++ ")"
  pretty (TyApp (TyCon (TupleConstructor _)) args) = "(" ++ intercalate ", " (map pretty (args)) ++ ")"
  pretty (TyApp con args) = "(" ++ unwords (map pretty (con : args)) ++ ")"
  pretty (TyVar s) = "(var " ++ pretty s ++ ")"
  pretty (TyAlias raw alias) = "(" ++ pretty alias ++ "(aliased as: " ++ pretty raw ++ "))"
  -- pretty (TyFrac (AbstractLiteral 1) address ty) = "Frac<" ++ pretty address ++ ", " ++ pretty frac ++ " * (" ++ pretty ty ++ ")>"
  pretty (Owned address ty) = "Owned<" ++ pretty address ++ ", " ++ pretty ty ++ ">"
  pretty (TyFrac frac address ty) = "Frac<" ++ pretty address ++ ", " ++ pretty frac ++ " * (" ++ pretty ty ++ ")>"
  pretty (TySchema qNames ty) = "∀ " ++ unwords (map pretty qNames) ++ ". " ++ pretty ty
  pretty (TyExists [qAddr] (Owned (VarAddress addr) ty)) | qAddr == addr = "Owned<∃ " ++ pretty addr ++ ", " ++ pretty ty ++ ">"
  pretty (TyExists [qAddr] (TyFrac frac (VarAddress addr) ty)) | qAddr == addr = "Frac<∃ " ++ pretty addr ++ ", " ++ pretty frac ++ " * (" ++ pretty ty ++ ")>"
  pretty (TyExists qNames ty) = "∃ " ++ unwords (map pretty qNames) ++ ". " ++ pretty ty

eqType :: HasCallStack => Type -> Type -> Bool
eqType ty1 ty2 = unificationSuccess (unify ty1 ty2)

-- instance Eq Type where
--   (==) ty1 ty2 = unificationSuccess (unify ty1 ty2)

returnType :: Type -> Type
returnType (TyFrac _ _ ty) = returnType ty
returnType (TyArrow _ _ ty) = ty
returnType (TySchema qNames ty) =
  let remainingQNames = qNames `intersect` getQuantifiedVars (returnType ty)
   in if null remainingQNames then returnType ty else TySchema remainingQNames (returnType ty)
returnType (TyExists qNames ty) =
  let remainingQNames = qNames `intersect` getQuantifiedVars (returnType ty)
   in if null remainingQNames then returnType ty else TyExists remainingQNames (returnType ty)

constructorFieldTypes :: Type -> [Type]
constructorFieldTypes (TyFrac _ _ ty) = constructorFieldTypes ty
constructorFieldTypes (TyArrow _ paramTys _) = paramTys
constructorFieldTypes (TySchema qNames innerTy) = do
  ty <- constructorFieldTypes innerTy
  let newQNames = qNames `intersect` getQuantifiedVars ty
  if null newQNames
    then
      return ty
    else
      return (TySchema newQNames ty)
constructorFieldTypes (TyExists qNames innerTy) = do
  ty <- constructorFieldTypes innerTy
  let newQNames = qNames `intersect` getQuantifiedVars ty
  if null newQNames
    then
      return ty
    else
      return (TyExists newQNames ty)

paramTypes :: Type -> [Type]
paramTypes (TyFrac _ _ ty) = paramTypes ty
paramTypes (TyArrow _ paramTys _) = paramTys
paramTypes ty@(TySchema qNames _) = error $ "Cannot get param types of quantified type: " ++ pretty ty
paramTypes ty@(TyExists qNames _) = error $ "Cannot get param types of quantified type: " ++ pretty ty

-- arity :: Type -> Int
-- arity (TyArrow ps _) = length ps
-- arity (TySchema _ ty) = arity ty
-- arity (TyExists _ ty) = arity ty
-- arity (TyFrac _ _ ty) = arity ty

-- (==) (TyCon c1) (TyCon c2) = c1 == c2
-- (==) (TyArrow captures1 args1 r1) (TyArrow captures2 args2 r2) = r1 == r2 && and (zipWith (==) args1 args2)
-- (==) (TyApp con1 args1) (TyApp con2 args2) = con1 == con2 && and (zipWith (==) args1 args2)
-- (==) (TyFrac frac1 address1 ty1) (TyFrac frac2 address2 ty2) = ty1 == ty2 && frac1 == frac2 && address1 == address2
-- (==) _ _ = False

-- type Substitution = ([(String, Type)], [(String, AbstractExpr)], [(String, CaptureEnv)], [(String, AbstractAddress)])

-- emptySubst :: Substitution
-- emptySubst = ([], [], [], [])

-- -- mergeSubst :: Substitution -> Substitution -> Substitution
-- -- mergeSubst (a1, a2, a3) (b1, b2, b3) = (a1 ++ b1, a2 ++ b2, a3 ++ b3)

-- toMaybe :: Bool -> a -> UnificationResult a
-- toMaybe True a = pure a
-- toMaybe False _ = MaybeT (pure Nothing)

-- -- unifyCaptures :: Substitution ->

-- type UnificationResult a = MaybeT (State Int) a

-- unifyCaptures :: Substitution -> CaptureEnv -> CaptureEnv -> UnificationResult Substitution
-- unifyCaptures = undefined

-- unifyAddresses :: Substitution -> AbstractAddress -> AbstractAddress -> UnificationResult Substitution
-- unifyAddresses = undefined

-- unifyFracs :: Substitution -> AbstractFrac -> AbstractFrac -> UnificationResult Substitution
-- unifyFracs = undefined

-- {--
--   b -> c -> Int -> b
--   a -> a -> a -> Int
-- --}

-- unifyTypes :: Substitution -> Type -> Type -> UnificationResult Substitution
-- unifyTypes _ (TyCon c1) (TyCon c2) = toMaybe (c1 == c2) emptySubst
-- unifyTypes subst (TyArrow captures1 args1 r1) (TyArrow captures2 args2 r2) = do
--   subst <- unifyTypes subst r1 r2
--   subst <- unifyCaptures subst captures1 captures2
--   foldM (\subst (arg1, arg2) -> unifyTypes subst arg1 arg2) subst (zip args1 args2)
-- unifyTypes subst (TyApp con1 args1) (TyApp con2 args2) = do
--   subst <- unifyTypes subst con1 con2
--   foldM (\subst (arg1, arg2) -> unifyTypes subst arg1 arg2) subst (zip args1 args2)
-- unifyTypes subst (TyFrac frac1 address1 ty1) (TyFrac frac2 address2 ty2) = do
--   subst <- unifyTypes subst ty1 ty2
--   subst <- unifyFracs subst frac1 frac2
--   unifyAddresses subst address1 address2
-- -- unifyTypes subst ()
--   -- ty1 == ty2 && frac1 == frac2 && address1 == address2
-- -- unifyTypes _ _ _ = False

-- -- sameType :: Type -> Type -> Bool
-- -- sameType (TyCon c1) (TyCon c2) = c1 == c2
-- -- sameType (TyArrow args1 r1) (TyArrow args2 r2) = sameType r1 r2 && and (zipWith sameType args1 args2)
-- -- sameType (TyApp con1 args1) (TyApp con2 args2) = sameType con1 con2 && and (zipWith sameType args1 args2)
-- -- sameType (TyFrac frac1 address1 ty1) (TyFrac frac2 address2 ty2) = sameType ty1 ty2 && frac1 == frac2 && address1 == address2
-- -- sameType _ _ = False

tuple :: [Type] -> Type
tuple types = TyApp (tupleType (length types)) types

rawTuple :: [RawType] -> RawType
rawTuple types = RawTyApp (toRaw (tupleType (length types))) types

type TySubstitution = (VarName, Type) -- [((String, Type), (String, AbstractFrac), (String, CaptureEnv), (String, AbstractAddress))]

type FracSub = (VarName, AbstractFrac)

type EnvSub = (VarName, CaptureEnv)

type AddressSub = (VarName, AbstractAddress)

type Substitution = ([TySubstitution], [FracSub], [EnvSub], [AddressSub])

type SkolemizationResult a = State [VarName] a

-- type ValTypeEnv = [String]

-- baseValTypeEnv :: ValTypeEnv
-- baseValTypeEnv = ["Bool", "String", "Int", "List"]

-- isValType :: ValTypeEnv -> Type -> Bool
-- isValType valTypes (TyCon name) = name `elem` valTypes
-- isValType _ Unit = True
-- isValType valTypes (Tuple _ vals) = all (isValType valTypes) vals
-- isValType valTypes (TySchema _ ty) = isValType valTypes ty
-- isValType _ (TyArrow (CaptureEnvVar _) _ _) = False
-- isValType valTypes (TyArrow (CaptureEnv env) _ _) = all (isValType valTypes) env
-- isValType valTypes (TyApp rator rands) = isValType valTypes rator && all (isValType valTypes) rands
-- isValType _ (TyVar _) = False
-- isValType _ (TyFrac {}) = False

-- isValTypeModVars :: ValTypeEnv -> Type -> Bool
-- isValTypeModVars _ (TyVar _) = True
-- isValTypeModVars valTypes ty = isValType valTypes ty

-- getFree :: Type -> [String]
-- getFree (TyVar var) = [var]
-- getFree (TyApp rator rands) = getFree rator ++ (rands >>= getFree)
-- getFree (TyArrow _ params r) = getFree r ++ (params >>= getFree)
-- getFree (TyFrac _ _ ty) = getFree ty
-- getFree (TySchema qNames ty) = getFree ty \\ qNames
-- getFree (TyExists qNames ty) = getFree ty \\ qNames
-- getFree (TyCon _) = []

occurs :: VarName -> Type -> Bool
occurs target (TyVar var) = var == target
occurs target (TyApp rator rands) = occurs target rator || any (occurs target) rands
occurs target (TyArrow env params r) = occurs target r || any (occurs target) params || occursEnv target env
occurs target (TyFrac frac addr ty) = occurs target ty || occursFrac target frac || occursAddr target addr
occurs target (TySchema qNames ty) = target `notElem` qNames && occurs target ty
occurs target (TyExists qNames ty) = target `notElem` qNames && occurs target ty
occurs target (TyAlias _ ty) = occurs target ty
occurs _ (TyCon _) = False

occursEnv :: VarName -> CaptureEnv -> Bool
occursEnv target (CaptureEnvVar v) = target == v
occursEnv _ _ = False

occursAddr :: VarName -> AbstractAddress -> Bool
occursAddr target (VarAddress v) = target == v
occursAddr _ _ = False

occursFrac :: VarName -> AbstractFrac -> Bool
occursFrac target f = target `elem` getQuantifiedVarsFrac f

renameEnv :: VarName -> VarName -> CaptureEnv -> CaptureEnv
renameEnv target replacement (CaptureEnvVar env) = CaptureEnvVar (renameVarName target replacement env)
renameEnv _ _ (InstantiatedCaptureEnvVar env) = InstantiatedCaptureEnvVar env
renameEnv target replacement (CaptureEnv captures) = CaptureEnv (map (rename target replacement) captures)

renameAddr :: VarName -> VarName -> AbstractAddress -> AbstractAddress
renameAddr _ _ addr@(FixedAddress _) = addr
renameAddr _ _ addr@(InstantiatedAddress _) = addr
-- renameAddr target replacement (UniqueAddress v) = UniqueAddress (renameStr target replacement v)
renameAddr target replacement (VarAddress v) = VarAddress (renameVarName target replacement v)

renameVarName :: VarName -> VarName -> VarName -> VarName
renameVarName old replacement target = if target == old then replacement else target

-- renameStr :: String -> String -> String -> String
-- renameStr old replacement target = if target == old then replacement else target

renameFrac :: VarName -> VarName -> AbstractFrac -> AbstractFrac
renameFrac target replacement (AbstractFrac fixed vars) = AbstractFrac fixed (mapFst (map (renameVarName target replacement)) vars)

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst f xs = zip (map (f . fst) xs) (map snd xs)

rename :: VarName -> VarName -> Type -> Type
rename target replacement (TyAlias raw aliased) = TyAlias raw (rename target replacement aliased)
rename target replacement (TyVar var) = TyVar (renameVarName target replacement var)
rename target replacement (TyApp rator rands) = TyApp (rename target replacement rator) (map (rename target replacement) rands)
rename target replacement (TyArrow env params r) = TyArrow (renameEnv target replacement env) (map (rename target replacement) params) (rename target replacement r)
rename target replacement (TyFrac frac addr ty) = TyFrac (renameFrac target replacement frac) (renameAddr target replacement addr) (rename target replacement ty)
rename target replacement (TySchema qNames ty)
  | target `notElem` qNames = TySchema qNames (rename target replacement ty)
  | otherwise = TySchema qNames ty
rename target replacement (TyExists qNames ty)
  | target `notElem` qNames = TyExists qNames (rename target replacement ty)
  | otherwise = TyExists qNames ty
rename _ _ ty@(TyCon _) = ty

removeElem :: (Eq a) => a -> [a] -> [a]
removeElem a = filter (a /=)

-- data UnificationError = ArityMismatch Int Int | InfiniteType TySubstitution

applyTySubst :: TySubstitution -> Type -> Type
applyTySubst (name, ty) _ | name `occurs` ty = error $ "Infinite type in subst: " ++ show (name, ty)
applyTySubst (name, ty) (TyVar var)
  | name == var = ty
  | otherwise = TyVar var
applyTySubst s (TyAlias raw aliased) = TyAlias raw (applyTySubst s aliased)
applyTySubst s (TyApp rator rands) = TyApp (applyTySubst s rator) (map (applyTySubst s) rands)
applyTySubst s (TyArrow env params r) = TyArrow env (map (applyTySubst s) params) (applyTySubst s r)
applyTySubst s (TyFrac frac address ty) = TyFrac frac address (applyTySubst s ty)
-- applyTySubst s@(substName, (TyVar newName)) (TySchema qNames ty) | substName `elem` qNames = TySchema (newName : (removeElem substName qNames)) (applyTySubst s ty)
applyTySubst (substName, _) ty@(TySchema _ _) | not $ substName `occursEver` ty = ty -- trace (substName ++ " does not occur in: " ++ pretty innerTy) ty
applyTySubst (substName, _) ty@(TyExists _ _) | not $ substName `occursEver` ty = ty -- trace (substName ++ " does not occur in: " ++ pretty innerTy) ty
applyTySubst s@(substName, substTy) t@(TySchema qNames ty) =
  let substQNames = getQuantifiedVars substTy
      newQnames = removeElem substName qNames ++ substQNames
      overlaps = substQNames `intersect` qNames
   in if not $ null overlaps
        then error $ "Overlapping qNames in substitution! " ++ show overlaps ++ " when applying substitution: " ++ show s ++ " to type" ++ pretty t
        else
          if null newQnames
            then applyTySubst s ty
            else TySchema newQnames (applyTySubst s ty)
-- applyTySubst s@(substName, (TyVar newName)) (TyExists qNames ty) | substName `elem` qNames = TyExists (newName : (removeElem substName qNames)) (applyTySubst s ty)
applyTySubst s@(substName, substTy) t@(TyExists qNames ty) =
  let substQNames = getQuantifiedVars substTy
      newQnames = removeElem substName qNames ++ substQNames
      overlaps = substQNames `intersect` qNames
   in if not $ null overlaps
        then error $ "Overlapping qNames in substitution! " ++ show overlaps ++ " when applying substitution: " ++ show s ++ " to type" ++ pretty t
        else
          if null newQnames
            then applyTySubst s ty
            else TyExists newQnames (applyTySubst s ty)
applyTySubst _ ty@(TyCon _) = ty

applyAddrSubst :: AddressSub -> Type -> Type
-- applyAddrSubst (name, ty) _ | name `occurs` ty = error "Infinite type"
-- applyAddrSubst (name, ty) (TyVar var)
--   | name == var = ty
--   | otherwise = TyVar var
applyAddrSubst s (TyApp rator rands) = TyApp (applyAddrSubst s rator) (map (applyAddrSubst s) rands)
applyAddrSubst s (TyArrow env params r) = TyArrow env (map (applyAddrSubst s) params) (applyAddrSubst s r)
applyAddrSubst s@(target, replacement) (TyFrac frac (VarAddress addr) ty) | addr == target = (TyFrac frac replacement (applyAddrSubst s ty))
applyAddrSubst s@(substName, VarAddress newName) (TySchema qNames ty) | substName `elem` qNames = TySchema (newName : (removeElem substName qNames)) (applyAddrSubst s ty)
applyAddrSubst s@(substName, _) (TySchema qNames ty) =
  let newQnames = removeElem substName qNames
   in if null newQnames
        then applyAddrSubst s ty
        else TySchema newQnames (applyAddrSubst s ty)
-- post skolemization there should be no issues with name conflicts
-- \| substName `notElem` qNames = TySchema qNames (applyTySubst s ty)
-- \| otherwise = TySchema qNames ty
applyAddrSubst _ ty = ty

applyEnvSubst :: EnvSub -> Type -> Type
applyEnvSubst s (TyApp rator rands) = TyApp (applyEnvSubst s rator) (map (applyEnvSubst s) rands)
applyEnvSubst s@(target, replacement) (TyArrow (CaptureEnvVar env) params r) | env == target = TyArrow replacement (map (applyEnvSubst s) params) (applyEnvSubst s r)
applyEnvSubst s (TyArrow env params r) = TyArrow env (map (applyEnvSubst s) params) (applyEnvSubst s r)
applyEnvSubst s (TyFrac frac address ty) = TyFrac frac address (applyEnvSubst s ty)
applyEnvSubst s@(substName, CaptureEnvVar newName) (TySchema qNames ty) | substName `elem` qNames = TySchema (newName : (removeElem substName qNames)) (applyEnvSubst s ty)
applyEnvSubst s@(substName, _) (TySchema qNames ty) =
  let newQnames = removeElem substName qNames
   in if null newQnames
        then applyEnvSubst s ty
        else TySchema newQnames (applyEnvSubst s ty)
applyEnvSubst s (TyExists qNames ty) = TyExists qNames (applyEnvSubst s ty)
applyEnvSubst _ ty = ty

mergeFracs :: [AbstractFrac] -> AbstractFrac
mergeFracs = foldr (|+|) (AbstractNum 0)

applyFracSubstHelper :: FracSub -> AbstractFrac -> AbstractFrac
applyFracSubstHelper (name, replacement) (AbstractFrac fixed vars) =
  AbstractFrac fixed [] |+| (mergeFracs (map helper vars))
  where
    helper :: ([AbstractId], Rational) -> AbstractFrac
    helper (vars, fixed) | name `elem` vars = replacement |*| (AbstractFrac Nothing [(removeElem name vars, fixed)])
    helper frac = (AbstractFrac Nothing [frac])

applyFracSubst :: FracSub -> Type -> Type
applyFracSubst s (TyApp rator rands) = TyApp (applyFracSubst s rator) (map (applyFracSubst s) rands)
applyFracSubst s (TyArrow env params r) = TyArrow env (map (applyFracSubst s) params) (applyFracSubst s r)
applyFracSubst s (TyFrac frac addr ty) = (TyFrac (applyFracSubstHelper s frac) addr (applyFracSubst s ty))
applyFracSubst s@(substName, frac) (TySchema qNames ty) | substName `elem` qNames = TySchema ((getQuantifiedVarsFrac frac) ++ (removeElem substName qNames)) (applyFracSubst s ty)
applyFracSubst s@(substName, _) (TySchema qNames ty) =
  let newQnames = removeElem substName qNames
   in if null newQnames
        then applyFracSubst s ty
        else TySchema newQnames (applyFracSubst s ty)
-- post skolemization there should be no issues with name conflicts
-- \| substName `notElem` qNames = TySchema qNames (applyTySubst s ty)
-- \| otherwise = TySchema qNames ty
applyFracSubst _ ty = ty

applySubsts :: Substitution -> Type -> Type
applySubsts (tySubsts, fracSubsts, envSubsts, addrSubsts) ty = 
  foldr applyTySubst (foldr applyFracSubst (foldr applyEnvSubst (foldr applyAddrSubst ty (reverse addrSubsts)) (reverse envSubsts)) (reverse fracSubsts)) (reverse tySubsts)

data UnificationError = TypeMismatch Type Type
                        | Circularity
                        | AddressMismatch AbstractAddress AbstractAddress 
                        | FracMismatch AbstractFrac AbstractFrac
                        | EnvMismatch CaptureEnv CaptureEnv
  deriving (Show)

instance Pretty UnificationError where
  pretty (TypeMismatch ty1 ty2) = "Couldn't unify types " ++ pretty ty1 ++ " and " ++ pretty ty2
  pretty (FracMismatch frac1 frac2) = "Couldn't unify fractions " ++ pretty frac1 ++ " and " ++ pretty frac2
  pretty (AddressMismatch addr1 addr2) = "Couldn't unify addresses " ++ pretty addr1 ++ " and " ++ pretty addr2
  pretty (EnvMismatch env1 env2) = "Couldn't unify environments " ++ pretty env1 ++ " and " ++ pretty env2

type UnificationResult a = ExceptT UnificationError (State Int) a

-- renameQNames :: [String] -> Type -> UnificationResult [Type]

emptySubst :: Substitution
emptySubst = ([], [], [], [])

tySubst :: [TySubstitution] -> Substitution
tySubst s = (s, [], [], [])

fracSubst :: [FracSub] -> Substitution
fracSubst s = ([], s, [], [])

envSubst :: [EnvSub] -> Substitution
envSubst s = ([], [], s, [])

addrSubst :: [AddressSub] -> Substitution
addrSubst s = ([], [], [], s)

unifyList :: [Type] -> [Type] -> UnificationResult Substitution
unifyList [] [] = return emptySubst
unifyList (p1 : r1) (p2 : r2) = do
  sub <- unifyTypes p1 p2
  restSub <- unifyList (map (applySubsts sub) r1) (map (applySubsts sub) r2)
  return $ sub `mergeSubst` restSub
-- unifyList [] []
unifyList ls1 ls2 = error $ "Arity mismatch " ++ show ls1 ++ " other " ++ show ls2

unifyAddr :: AbstractAddress -> AbstractAddress -> UnificationResult Substitution
unifyAddr addr1@(FixedAddress a1) addr2@(FixedAddress a2)
  | a1 == a2 = return emptySubst
  | otherwise = throwError (AddressMismatch addr1 addr2)
-- unifyAddr addr1@(VarAddress a1) addr2@(UniqueAddress a2) = throwError (AddressMismatch addr1 addr2)
-- unifyAddr addr1@(UniqueAddress a1) addr2@(VarAddress a2) = throwError (AddressMismatch addr1 addr2)
unifyAddr (VarAddress a1) (VarAddress a2) | a1 == a2 = return emptySubst
unifyAddr (VarAddress a1) addr = return $ addrSubst [(a1, addr)]
unifyAddr addr (VarAddress a1) = return $ addrSubst [(a1, addr)]
-- unifyAddr addr@(FixedAddress _) (UniqueAddress a1) = return $ addrSubst [(a1, addr)]
-- unifyAddr (UniqueAddress a1) addr@(FixedAddress _) = return $ addrSubst [(a1, addr)]
-- unifyAddr (UniqueAddress _) (UniqueAddress _) = return emptySubst
unifyAddr addr1@(InstantiatedAddress a1) addr2@(InstantiatedAddress a2)
  | a1 == a2 = return emptySubst
  | otherwise = throwError (AddressMismatch addr1 addr2)
unifyAddr addr1 addr2 = throwError (AddressMismatch addr1 addr2)

unifyEnv :: CaptureEnv -> CaptureEnv -> UnificationResult Substitution
unifyEnv (CaptureEnv captures1) (CaptureEnv captures2) = unifyList captures1 captures2
unifyEnv env1@(InstantiatedCaptureEnvVar v1) env2@(InstantiatedCaptureEnvVar v2)
  | v1 == v2 = return emptySubst
  | otherwise = throwError (EnvMismatch env1 env2)
unifyEnv (CaptureEnvVar v1) (CaptureEnvVar v2) | v1 == v2 = return emptySubst
unifyEnv (CaptureEnvVar v1) env = return $ envSubst [(v1, env)]
unifyEnv env (CaptureEnvVar v1) = return $ envSubst [(v1, env)]

unifyFrac :: AbstractFrac -> AbstractFrac -> UnificationResult Substitution
unifyFrac frac1 frac2 | frac1 == frac2 = return $ fracSubst []
unifyFrac (AbstractFrac Nothing [([name], 1)]) frac = return $ fracSubst [(name, frac)]
unifyFrac frac (AbstractFrac Nothing [([name], 1)]) = return $ fracSubst [(name, frac)]
unifyFrac frac1 frac2 = throwError (FracMismatch frac1 frac2)

mergeSubst :: Substitution -> Substitution -> Substitution
mergeSubst (a1, b1, c1, d1) (a2, b2, c2, d2) = (a1 ++ a2, b1 ++ b2, c1 ++ c2, d1 ++ d2)

-- pattern QualifiedType

unifyTypes :: Type -> Type -> UnificationResult Substitution
unifyTypes t1@(TyCon c1) t2@(TyCon c2)
  | c1 == c2 = return emptySubst
  | otherwise = throwError (TypeMismatch t1 t2)
unifyTypes ty1@(TyArrow env1 params1 r1) ty2@(TyArrow env2 params2 r2) = do
  unless (length params1 == length params2) (throwError $ TypeMismatch ty1 ty2)
  envSubst <- unifyEnv env1 env2
  substs1 <- unifyList params1 params2
  substs2 <- unifyTypes (applySubsts substs1 r1) (applySubsts substs1 r2)
  return $ envSubst `mergeSubst` substs1 `mergeSubst` substs2
unifyTypes ty1@(TyApp rator1 rands1) ty2@(TyApp rator2 rands2) = do
  -- trace (show rator1 ++ " other " ++ show rator2) (return ())
  -- trace (show rands1 ++ " other " ++ show rands2) (return ())
  unless (length rands1 == length rands2) (throwError $ TypeMismatch ty1 ty2)
  substs1 <- unifyList rands1 rands2
  substs2 <- unifyTypes (applySubsts substs1 rator1) (applySubsts substs1 rator2)
  return $ substs2 `mergeSubst` substs1
unifyTypes (TyFrac frac1 addr1 ty1) (TyFrac frac2 addr2 ty2) = do
  fracSubst <- unifyFrac frac1 frac2
  addrSubst <- unifyAddr addr1 addr2
  subst <- unifyTypes ty1 ty2
  return $ fracSubst `mergeSubst` addrSubst `mergeSubst` subst
unifyTypes (TyVar x) t@(TyVar y)
  | x == y = return emptySubst
  | otherwise = return $ tySubst [(x, t)]
unifyTypes (TyVar x) ty | x `occurs` ty = throwError Circularity
                        | otherwise = return $ tySubst [(x, ty)]
unifyTypes ty (TyVar x) | x `occurs` ty = throwError Circularity
                        | otherwise = return $ tySubst [(x, ty)]
unifyTypes ty1@(TyExists _ t1) ty2@(TyExists _ t2) = do
  -- traceM ("unifying: " ++ pretty ty1 ++ " with " ++ pretty ty2)
  unifyTypes t1 t2
-- unifyTypes (TySchema _ t1) (TyExists _ t2) = unifyTypes t1 t2
unifyTypes (TySchema _ t1) t2 = unifyTypes t1 t2
unifyTypes t1 (TySchema _ t2) = unifyTypes t1 t2
unifyTypes (TyAlias _ t1) (TyAlias _ t2) = unifyTypes t1 t2
unifyTypes t1 (TyAlias _ t2) = unifyTypes t1 t2
unifyTypes (TyAlias _ t1) t2 = unifyTypes t1 t2
unifyTypes ty1 ty2 = throwError (TypeMismatch ty1 ty2)

-- mergeBindings :: Type -> Type -> Type
-- mergeBindings (TySchema qNames ty1) ty2 = TySchema qNames (mergeBindings ty1 ty2)
-- mergeBindings (TyExists qNames ty1) ty2 = TyExists qNames (mergeBindings ty1 ty2)
-- mergeBindings ty1 (TySchema qNames ty2) = TySchema qNames (mergeBindings ty1 ty2)
-- mergeBindings ty1 (TyExists qNames ty2) = TyExists qNames (mergeBindings ty1 ty2)
-- mergeBindings ty1 ty2 = ty1

-- unconstructor :: Type -> Type -> UnificationResult (Type, Substitution)
-- unconstructor constructorType resultType = undefined

-- unifyApplication :: Type -> [Type] -> UnificationResult Substitution
-- unifyApplication (TyFrac _ _ arrowTys) argTypes =
-- let paramTys
-- undefined

type OverallUnificationResult a = Either (Type, Type, UnificationError) a

-- type OverallUnificationResult a = Either (Type, Type, UnificationError) a

liftUnificationResult :: Type -> Type -> UnificationResult a -> OverallUnificationResult a
liftUnificationResult ty1 ty2 res = do
  let val = evalState (runExceptT res) 0
  case val of
    (Right a) -> return a
    (Left err) -> throwError (ty1, ty2, err)

getCallerLoc :: HasCallStack => String
getCallerLoc = prettyCallStack (popCallStack callStack)

unify :: HasCallStack => Type -> Type -> OverallUnificationResult (Type, Substitution)
unify t1Raw t2Raw = do
  -- traceM ("caller is: " ++ show getCallerLineNum ++ " uhhh " ++ pretty ty1)
  -- traceM ("unifying:" ++ pretty ty1)
  -- traceM ("with: " ++ pretty ty2)
  unification <- liftUnificationResult ty1 ty2 (unifyTypes ty1 ty2)
  -- traceM ("applying subst: " ++ pretty unification)
  -- let ty1' = applySubsts unification ty1
  -- let ty2' = applySubsts unification ty2
  let ty1' = applySubsts unification ty1
  -- let ty2' = applySubsts unification ty2
  -- traceM ("subst applied! "  ++ pretty unification)
  -- let resType = mergeBindings ty1' ty2'
  -- traceM ("getting qnames in: "  ++ pretty ty1')
  let qNames = getQuantifiedVars ty1'
  -- traceM ("got qnames! " ++ pretty qNames)
  if null qNames then return (ty1', unification) else error $ "Escaped TyVar(s) " ++ show qNames ++ " in: " ++ pretty ty1' ++ " unifying: " ++ pretty ty1 ++ " and " ++ pretty ty2 ++ " with subst " ++ show unification
  where
    -- return (ty1', unification)
    (ty1, ty2) =
      evalState
        ( do
            t1 <- skolemize t1Raw
            t2 <- skolemize t2Raw
            return (t1, t2)
        )
        []

{--

[VarName "addr" 0] in: 

∀ a[1] env[1]. 
  Owned<1, 
    ((Owned<addr[0], (var a[1])>(aliased as: RawTyApp (RawTyCon "Owned") [RawTyVar (VarName "#unique" 0),RawTyVar (VarName "a" 0)]))(aliased as: RawTyApp (RawTyCon "UniqueOwned") [RawTyArrow (CaptureEnvVar (VarName "env" 0)) [RawTyApp (RawTyCon "Tuple:0") []] (RawTyVar (VarName "a" 0))])) -> ((Owned<addr[0], (var a[1])>(aliased as: RawTyApp (RawTyCon "Owned") [RawTyVar (VarName "#unique" 0),RawTyVar (VarName "a" 0)]))(aliased as: RawTyApp (RawTyCon "UniqueOwned") [RawTyApp (RawTyCon "Maybe") [RawTyVar (VarName "a" 0)]])) -> ∃ addr[0] a[1] env[1]. Owned<addr[0], (#Thunk (var env[1]) (var a[1]))>> 
    
unifying: 
  ∀ env[0] a[0]. Owned<1, ((Owned<addr[0], (var a[0])>(aliased as: RawTyApp (RawTyCon "Owned") [RawTyVar (VarName "#unique" 0),RawTyVar (VarName "a" 0)]))(aliased as: RawTyApp (RawTyCon "UniqueOwned") [RawTyArrow (CaptureEnvVar (VarName "env" 0)) [RawTyApp (RawTyCon "Tuple:0") []] (RawTyVar (VarName "a" 0))])) -> ((Owned<addr[0], (var a[0])>(aliased as: RawTyApp (RawTyCon "Owned") [RawTyVar (VarName "#unique" 0),RawTyVar (VarName "a" 0)]))(aliased as: RawTyApp (RawTyCon "UniqueOwned") [RawTyApp (RawTyCon "Maybe") [RawTyVar (VarName "a" 0)]])) -> Owned<∃ addr[0], (#Thunk (var env[0]) (var a[0]))>> 
and 
  ∀ env[1] a[1]. Owned<1, ((Owned<addr[0], (var a[1])>(aliased as: RawTyApp (RawTyCon "Owned") [RawTyVar (VarName "#unique" 0),RawTyVar (VarName "a" 0)]))(aliased as: RawTyApp (RawTyCon "UniqueOwned") [RawTyArrow (CaptureEnvVar (VarName "env" 0)) [RawTyApp (RawTyCon "Tuple:0") []] (RawTyVar (VarName "a" 0))])) -> ((Owned<addr[0], (var a[1])>(aliased as: RawTyApp (RawTyCon "Owned") [RawTyVar (VarName "#unique" 0),RawTyVar (VarName "a" 0)]))(aliased as: RawTyApp (RawTyCon "UniqueOwned") [RawTyApp (RawTyCon "Maybe") [RawTyVar (VarName "a" 0)]])) -> Owned<∃ addr[1], (#Thunk (var env[1]) (var a[1]))>> 
    
with subst ([(VarName "a" 0,TyVar (VarName "a" 1)),(VarName "env" 0,TyVar (VarName "env" 1))],[],[],[(VarName "addr" 0,VarAddress (VarName "addr" 1))])

--}

genNameHelper ::  [VarName] -> VarName -> VarName
genNameHelper names name = if name `notElem` names then name else genNameHelper names (inc name)

genName :: VarName -> SkolemizationResult VarName
genName name = do
  usedNames <- get
  let newName = genNameHelper usedNames name
  put (newName : usedNames)
  return newName
  -- case name `elem`  of
  --   Just n -> do
  --     put ((name, n + 1) : dict)
  --     return $ name ++ show n
  --   Nothing -> do
  --     put ((name, 0) : dict)
  --     return name

-- instantiateTy :: Type -> Type
-- instantiateTy ty = Skolemize

-- skolemizeFrac :: AbstractFrac -> SkolemizationResult AbstractFrac
-- skolemizeFrac f = return f

-- skolemizeAddr :: AbstractAddress -> SkolemizationResult AbstractAddress
-- skolemizeAddr (VarAddress a) = genName a <&> VarAddress
-- skolemizeAddr addr = return addr

instantiateAddr :: AbstractAddress -> AbstractAddress
instantiateAddr (VarAddress a) = (InstantiatedAddress a)
instantiateAddr addr = addr

skolemize :: Type -> SkolemizationResult Type
skolemize (TySchema qNames ty) = do
  gennedNames <- mapM genName qNames
  ty <- skolemize (foldr (\(old, new) ty -> rename old new ty) ty (zip qNames gennedNames))
  return $ TySchema gennedNames ty
skolemize (TyExists qNames ty) = do
  gennedNames <- mapM genName qNames
  ty <- skolemize (foldr (\(old, new) ty -> rename old new ty) ty (zip qNames gennedNames))
  return $ TyExists gennedNames ty
skolemize (TyApp rator rands) = do
  rator <- skolemize rator
  rands <- mapM skolemize rands
  return $ TyApp rator rands
skolemize (TyArrow env params r) = do
  r <- skolemize r
  params <- mapM skolemize params
  return $ TyArrow env params r
skolemize (TyFrac frac addr ty) = do
  ty <- skolemize ty
  return $ TyFrac frac addr ty
skolemize (TyAlias raw aliased) = do
  aliased <- skolemize aliased
  return (TyAlias raw aliased) 
skolemize ty@(TyCon _) = return ty
skolemize ty@(TyVar _) = return ty

instantiate :: Type -> SkolemizationResult Type
instantiate (TySchema qNames ty) = do
  gennedNames <- mapM genName qNames
  -- _ <- traceM ("genned names (schema): " ++ show gennedNames)
  -- traceM ("instantiated schema " ++ pretty (foldr (\(old, new) ty -> rename old new ty) ty (zip qNames gennedNames)))
  instantiate (foldr (\(old, new) ty -> rename old new ty) ty (zip qNames gennedNames))
instantiate (TyExists qNames ty) = do
  gennedNames <- mapM genName qNames
  -- _ <- traceM ("genned names (exists): " ++ show gennedNames)
  ty <- instantiate (foldr (\(old, new) ty -> rename old new ty) ty (zip qNames gennedNames))
  return $ TyExists gennedNames ty
instantiate (TyApp rator rands) = do
  rator <- instantiate rator
  rands <- mapM instantiate rands
  return $ TyApp rator rands
instantiate (TyArrow env params r) = do
  r <- instantiate r
  env <- instantiateEnv env
  params <- mapM instantiate params
  return $ TyArrow env params r
instantiate (TyFrac frac addr ty) = do
  -- frac <- skolemizeFrac frac
  ty <- instantiate ty
  return $ TyFrac frac addr ty
instantiate ty@(TyCon _) = return ty
instantiate ty@(TyVar n) = return $ TyCon (pretty n)

instantiateEnv :: CaptureEnv -> SkolemizationResult CaptureEnv
instantiateEnv (CaptureEnvVar v) = return $ InstantiatedCaptureEnvVar v
instantiateEnv (CaptureEnv v) = return $ CaptureEnv v

runUnificationResult :: OverallUnificationResult a -> a
runUnificationResult = \case
  (Right x) -> x
  (Left err) -> error (show err)

unify' :: Type -> Type -> (Type, Substitution)
unify' x y =
  runUnificationResult $ unify x y

-- test = unify'
--   (TySchema ["a", "b"] (TyArrow (CaptureEnv []) [TyVar "a", TyVar "b"] (TyCon "Unit")))
--   (TySchema ["a"] (TyArrow (CaptureEnv []) [TyCon "int", TyCon "int"] (TyVar "a")))

unificationSuccess :: OverallUnificationResult a -> Bool
unificationSuccess = isRight

-- trimQNames :: Type -> Type
-- trimQNames (TySchema qNames ty) = TySchema (qNames `intersect` (getQuantifiedVars ty)) ty
-- trimQNames (TyExists qNames ty) = TyExists (qNames `intersect` (getQuantifiedVars ty)) ty
-- trimQNames ty = ty

freeIn :: VarName -> Type -> Bool
freeIn s ty = s `elem` (getQuantifiedVars ty)

notFreeIn :: VarName -> Type -> Bool
notFreeIn s ty = not (freeIn s ty)

freeInRaw :: VarName -> RawType -> Bool
freeInRaw s ty = s `elem` (getQuantifiedVarsRaw ty)

notFreeInRaw :: VarName -> RawType -> Bool
notFreeInRaw s ty = not (freeInRaw s ty)

genQName' :: String -> Type -> VarName
genQName' s = genQName (FreshVarName s)

genQName :: VarName -> Type -> VarName
genQName name ty | name `notFreeIn` ty = name
genQName name ty = loop name
  where
    loop :: VarName -> VarName
    loop name = if (name `notFreeIn` ty) then name else loop (inc name)

genQNameRaw :: VarName -> RawType -> VarName
genQNameRaw name ty | name `notFreeInRaw` ty = name
genQNameRaw name ty = loop name
  where
    loop :: VarName -> VarName
    loop name = if (name `notFreeInRaw` ty) then name else loop (inc name)

genQNameRawList :: VarName -> [RawType] -> VarName
genQNameRawList name [] = name
genQNameRawList name (ty:tys) = genQNameRawList (genQNameRaw name ty) tys

-- getQuantifiedVars :: Type -> [VarName]

getQuantifiedVarsRaw :: RawType -> [VarName]
getQuantifiedVarsRaw (RawTyVar s) = [s]
getQuantifiedVarsRaw (RawTyCon _) = []
getQuantifiedVarsRaw (RawTyApp rator rands) = getQuantifiedVarsRaw rator `union` (foldr (union . getQuantifiedVarsRaw) [] rands)
getQuantifiedVarsRaw (RawTyArrow (CaptureEnvVar v) params r) = v : getQuantifiedVarsRaw r `union` (foldr (union . getQuantifiedVarsRaw) [] params)
getQuantifiedVarsRaw (RawTyArrow _ params r) = getQuantifiedVarsRaw r `union` (foldr (union . getQuantifiedVarsRaw) [] params)
getQuantifiedVarsRaw (RawTySchema qNames ty) = getQuantifiedVarsRaw ty \\ qNames
getQuantifiedVarsRaw (RawTyExists qNames ty) = getQuantifiedVarsRaw ty \\ qNames
getQuantifiedVarsRaw (RawTyFrac var addr ty) = (getQuantifiedVarsRaw ty) `union` (getQuantifiedVarsFrac var) `union` (getQuantifiedVarsAddress addr)

getQuantifiedVars :: Type -> [VarName]
getQuantifiedVars (TyVar s) = [s]
getQuantifiedVars (TyCon _) = []
getQuantifiedVars (TyApp rator rands) = getQuantifiedVars rator `union` (foldr (union . getQuantifiedVars) [] rands)
getQuantifiedVars (TyArrow (CaptureEnvVar v) params r) = v : getQuantifiedVars r `union` (foldr (union . getQuantifiedVars) [] params)
getQuantifiedVars (TyArrow _ params r) = getQuantifiedVars r `union` (foldr (union . getQuantifiedVars) [] params)
getQuantifiedVars (TySchema qNames ty) = getQuantifiedVars ty \\ qNames
getQuantifiedVars (TyExists qNames ty) = getQuantifiedVars ty \\ qNames
getQuantifiedVars (TyAlias rawTy ty) = getQuantifiedVars ty
getQuantifiedVars (TyFrac var addr ty) = (getQuantifiedVars ty) `union` (getQuantifiedVarsFrac var) `union` (getQuantifiedVarsAddress addr)

occursEverFrac :: VarName -> AbstractFrac -> Bool
occursEverFrac s frac = s `elem` getQuantifiedVarsFrac frac

occursEverAddr :: VarName -> AbstractAddress -> Bool
occursEverAddr s addr = s `elem` getQuantifiedVarsAddress addr

occursEver :: VarName -> Type -> Bool
occursEver s (TyVar s1) = s == s1
occursEver _ (TyCon _) = False
occursEver s (TyApp rator rands) = any (occursEver s) rands || (occursEver s rator)
occursEver s (TyArrow (CaptureEnvVar v) params r) = s == v || any (occursEver s) params || (occursEver s r)
occursEver s (TyArrow _ params r) = any (occursEver s) params || (occursEver s r)
occursEver s (TySchema _ ty) = occursEver s ty
occursEver s (TyExists _ ty) = occursEver s ty
occursEver s (TyAlias _ ty) = occursEver s ty
occursEver s (TyFrac frac addr ty) = (occursEverFrac s frac) || (occursEverAddr s addr) || (occursEver s ty)

-- getQuantifiedVars (TyFrac _ (VarAddress v) ty) = v : (getQuantifiedVars ty)
-- getQuantifiedVars (TyFrac var _ ty) = f : (getQuantifiedVars ty)
-- getQuantifiedVars (TyFrac _ _ ty) = getQuantifiedVars ty

getQuantifiedVarsAddress :: AbstractAddress -> [VarName]
getQuantifiedVarsAddress (VarAddress v) = [v]
getQuantifiedVarsAddress _ = []

getQuantifiedVarsFrac :: AbstractFrac -> [VarName]
getQuantifiedVarsFrac (AbstractFrac _ components) = join $ map fst components
getQuantifiedVarsFrac _ = []

-- test =
--   unify'
--     (TySchema ["a", "b"] (TyArrow (CaptureEnv []) [(TyVar "a"), (TyVar "b")] (tuple [(TyVar "a"), (TyVar "b")])))
--     (TySchema ["a", "b"] (TyArrow (CaptureEnv []) [(TyVar "a"), (TyVar "b")] (tuple [(TyCon "int"), (TyCon "bool")])))

-- test =
--   unify'
--     (TySchema ["a", "b"] (TyArrow (CaptureEnv []) [(TyFrac (abstract 1) (VarAddress "a") (TyVar "b")), (TyVar "b")] (TyCon "Unit")))
--     (TySchema ["a"] (TyArrow (CaptureEnv []) [(TyFrac (abstract 1) (FixedAddress 1) (TyCon "Int")), (TyCon "Int")] (TyVar "a")))

