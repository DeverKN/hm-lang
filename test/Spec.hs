{-# LANGUAGE PatternSynonyms #-}
import EzTest (TestResult, assert, handleIOTests, missing)
import LinearTypeChecker (checkFile)
import Types (CaptureEnv (..), Type (..), tuple, unify', pattern FreshVarName, VarName (VarName))

main :: IO ()
main = handleIOTests [{-- pure test, pure test2, pure test3, pure test4, pure test5, pure test8, plusTC, idTC, badIdTC, makeRefTC, listTC, --} dropListTC, tupleTC, untupleTC, dllTC, typeAliasTC, unboundTyVarTC]

test :: TestResult
test = assert "test" (words "the quick brown fox jumped over the lazy blue dog") ["the", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "blue", "dog"]

test2 :: TestResult
test2 = assert "test2" ("a" : ["b"]) ["a", "b"]

test3 :: TestResult
test3 = assert "test3" ((2 * 6) :: Int) (12 :: Int)

test4 :: TestResult
test4 =
  assert
    "unify"
    ( fst
        ( unify'
            (TySchema [FreshVarName "a", FreshVarName "b"] (TyArrow (CaptureEnv []) [TyVar $ FreshVarName "a", TyVar $ FreshVarName "b"] (tuple [TyVar $ FreshVarName "a", TyVar $ FreshVarName "b"])))
            (TySchema [FreshVarName "a", FreshVarName "b"] (TyArrow (CaptureEnv []) [TyVar $ FreshVarName "a", TyVar $ FreshVarName "b"] (tuple [TyCon "int", TyCon "bool"])))
        )
    )
    (TyArrow (CaptureEnv []) [TyCon "int", TyCon "bool"] (TyApp (TyCon "Tuple:2") [TyCon "int", TyCon "bool"]))

{--

E a1 . Int -> a1
A a2 . Int -> a2

[a1 -> a2]

--}
test5 :: TestResult
test5 = assert "existential types" (fst $ unify' (TyArrow (CaptureEnv []) [TyCon "Int"] (TyExists [FreshVarName "a"] (TyVar $ FreshVarName "a"))) (TySchema [FreshVarName "a"] (TyArrow (CaptureEnv []) [TyCon "Int"] (TyVar $ FreshVarName "a")))) (TyArrow (CaptureEnv []) [TyCon "Int"] (TyExists [VarName "a" 0] (TyVar (VarName "a" 0))))

test8 :: TestResult
test8 = assert "existential types 2" (fst $ unify' (TySchema [FreshVarName "a"] (TyArrow (CaptureEnv []) [TyCon "Int"] (TyVar $ FreshVarName "a"))) (TyArrow (CaptureEnv []) [TyCon "Int"] (TyExists [FreshVarName "a"] (TyVar $ FreshVarName "a")))) (TyArrow (CaptureEnv []) [TyCon "Int"] (TyExists [VarName "a" 1] (TyVar (VarName "a" 1))))

-- assertTypeCheck fileName expectedTy = do
--     ty <- checkFile "tests/Plus.hm"
--     return $ assert "plus typecheck" ty ("Type checking succeded with type of: Number")

plusTC :: IO TestResult
plusTC = do
  ty <- checkFile "tests/Plus.hm"
  return $ assert "plus typecheck" ty "Type checking succeded with type of: Number"

makeRefTC :: IO TestResult
makeRefTC = do
  ty <- checkFile "tests/MakeRef.hm"
  return $ assert "makeref typecheck" ty "Type checking succeded with type of: ∃ addr2. Frac<1, addr2, (Ref String)>"

idTC :: IO TestResult
idTC = do
  ty <- checkFile "tests/Id.hm"
  return $ assert "id typecheck" ty "Type checking succeded with type of: ∀ a. Frac<1, 0, [] (var a) -> (var a)>"

badIdTC :: IO TestResult
badIdTC = do
  ty <- checkFile "tests/BadId.hm"
  return $ assert "bad id typecheck" ty "Type checking failed with error: Type mismatch, expected: a but got Number"

listTC :: IO TestResult
listTC = do
  ty <- checkFile "tests/List.hm"
  return $ assert "list typecheck" ty "Type checking succeded with type of: ∃ addr3. Frac<1, addr3, (List Number)>"

dropListTC :: IO TestResult
dropListTC = do
  ty <- checkFile "tests/DropList.hm"
  return $ assert "drop list typecheck" ty "Type checking succeded with type of: Owned<∃ addr[0], ()>"

tupleTC :: IO TestResult
tupleTC = do
  ty <- checkFile "tests/Tuple.hm"
  return $ assert "tuple typecheck" ty "Type checking succeded with type of: Owned<∃ addr[0], (#Number, #Number)>"

untupleTC :: IO TestResult
untupleTC = do
  ty <- checkFile "tests/UnTuple.hm"
  return $ assert "untuple typecheck" ty "Type checking succeded with type of: #Number"

dllTC :: IO TestResult
dllTC = do
  ty <- checkFile "tests/DLL.hm"
  return $ assert "dll typecheck" ty "∀ a[0]. Owned<0, Frac<∃ addr[0], 1 % 2 * ((#DLL (var a[0])))> -> Owned<∃ addr[0], (#Ref (var a[0]))> -> Frac<∃ addr[0], 1 % 2 * ((#DLL (var a[0])))> -> Owned<∃ addr[0], (#DLL (var a[0]))>>"

typeAliasTC :: IO TestResult
typeAliasTC = do
  ty <- checkFile "tests/TypeAlias.hm"
  return $ assert "type alias typecheck" ty "Type checking succeded with type of: Owned<∃ addr[0], ()>"

unboundTyVarTC :: IO TestResult
unboundTyVarTC = do
  ty <- checkFile "tests/UnboundTyVar.hm"
  return $ assert "unbound tyvar typecheck" ty missing
{--

∀ a env a0. Frac<1, 1 * ([] | ∃ addr. Frac<addr, 1 * ((<#con>List (var a0)))> -> ∃ addr0. Frac<addr0, 1 * (<#var-env>env | (var a0) -> Unit)> -> Unit)>

∀ a1 b c d. Frac<d, c * (<#var-env>a1 | ∃ addr0. Frac<addr0, 1 * ((<#con>List <#con>a0))> -> ∃ a2. Frac<a2, 1 * (<#fixed>env | <#con>a0 -> Unit)> -> (var b))>
--}
-- test6 :: TestResult
-- test6 = assert "existential types" (fst $ unify' (TyExists ["a"] (TyVar "a")) (TySchema ["a"] (TyVar "a"))) (TyExists ["a0"] (TyVar "a0"))

-- existentialAddress :: TestResult
-- existentialAddress = assert "existential address" (fst $ unify' (TyExists ["a"] (TyFrac (AbstractNum 1) (VarAddress "a") (Ref (TyCon "Int")))) (TyExists ["a"] (TyFrac (AbstractNum 1) (VarAddress "a") (Ref (TyCon "Int"))))) missing
{--

exists a . Int -> a
forall a . Int -> a
=>
exists a . Int -> a

forall a . [Int] -> (Int -> a) -> [a]
exists a . [Int] -> (Int -> a) -> [a]

exists a . (Int, a) ~> forall a . a => exists a . (Int, a)
exists a . (Int, a) ~> (Int, a) =>

exists a . Int -> a ~> forall a b . a -> b => exists a . Int -> a
exists a . Int -> a ~> Int -> String => ⊥

exists a . Int -> a ~> exists a . a -> Int =>

data RList t = RNode (exists a . <a, 1, Ref t>) (RList t) | RNil
data RList t = RNode <#unique, 1, Ref t> (RList t) | RNil

data RList2 t = RNode <#unique a, .5, Ref t> <#unique a, .5, Ref t> (RList t) | RNil
(exists a . <a, .5, Ref t>) -> (exists a . <a, .5, Ref t>) -> (RList t) -> (RList t)
exists a . <a, .5, Ref t> -> <a, .5, Ref t> -> (RList t) -> (RList t)

-- data RList t = RNode <#unique, 1, Ref t> (RList t) | RNil

(exists a . <a, 1, Ref t>) -> (RList t) -> (RList t)

exists a1 . (<a1, 1, Ref t> -> (RList t) -> (RList t))

forall a2 . <1, 1, Ref t> -> RList t -> a

--}
-- myTest :: TestResult
-- myTest = assert "plus works" ("cat"++"dog") "catdog"

-- myTest2 :: TestResult
-- myTest2 = assert "unify" (fst (unify' (TyArrow (CaptureEnv []) [TyCon "Int"] (TyCon "Int")) (TySchema ["t"] (TyArrow (CaptureEnvVar []) [TyVar "t"] (TyVar "t"))))) (TyArrow (CaptureEnv []) [TyCon "Int"] (TyCon "Int"))
