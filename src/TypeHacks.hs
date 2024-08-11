{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE IncoherentInstances #-}

-- {-# LANGUAGE BangPatterns #-}
module TypeHacks where

import Data.Coerce (coerce)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl), (:~~:))
import GHC.Base (IP (..))
import GHC.Classes (IP)
import GHC.IORef (IORef)
import GHC.Types (Constraint)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (print)
import GHC.Types (Symbol)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (pattern SSymbol)
import Data.List (intercalate)

-- newtype Print a = Print {print :: a -> String}

-- printList :: Print a -> Print [a]
-- printList (Print print) = Print (unwords . map print)

-- display :: (?p :: Print a) => a -> IO ()
-- display a = putStrLn $ print ?p a

-- printShow :: (Show a) => Print a
-- printShow = Print show

-- instance Show a => IP "p" (Print a) where
--   ip = printShow

data Dict (c :: Constraint) where
  Dict :: (c) => Dict c

newtype ShowInt = ShowInt Int

instance Show ShowInt where
  show (ShowInt 0) = "zero"
  show (ShowInt n) = "nonzero"

showIntDict :: Dict (Show ShowInt)
showIntDict = Dict

coerceEq :: (a ~ b) => a -> b
coerceEq a = a

-- woah' :: Int :~: Bool -> Int -> Bool
-- woah' Refl = coerceEq

-- test :: a :~: ShowInt
-- test = Refl

unsafeIntDict :: Dict (Show Int)
unsafeIntDict = unsafeCoerce showIntDict

-- instance IP "p" (Print Int) where
--   ip = printShow

-- woah :: IO ()
-- woah = let ?p = Print (\x -> "Int: " ++ show x) in display 5

spooky :: String
spooky = case unsafeIntDict of Dict -> show (5 :: Int)

-- observeType :: a -> Bool

-- weird :: Eq b => b -> Bool
-- weird x = inner x
--   where
--     inner :: Eq b => b -> Bool
--     inner y = x == y

polyRef :: IORef [a]
{-# NOINLINE polyRef #-}
polyRef = unsafePerformIO $ newIORef []

{-# NOINLINE coerce' #-}
coerce' :: a -> b
coerce' a = unsafePerformIO $ do
  writeIORef polyRef [a]
  [b] <- readIORef polyRef
  return b

-- writeMaybe :: IORef (Maybe String) -> String -> IO ()
-- writeMaybe ref newVal = do
--   val <- readIORef ref
--   case val of
--     Nothing -> writeIORef ref (Just newVal)
--     Just _ -> return ()

-- weird :: (a, a) -> ((a, a) -> c) -> IO c
-- weird (a, b) f = do
--   accessedFirst <- newIORef Nothing
--   refA <- newIORef a
--   refB <- newIORef b
--   a' <- unsafeInterleaveIO (readIORef refA >>= \val -> writeIORef refB a >> putStrLn "read a" >> return val)
--   b' <- unsafeInterleaveIO (readIORef refB >>= \val -> writeIORef refA b >> putStrLn "read b" >> return val)
--   -- a' <- unsafeInterleaveIO (readIORef refA >>= \val -> writeMaybe accessedFirst "a" >> putStrLn "read a" >> return val)
--   -- b' <- unsafeInterleaveIO (readIORef refB >>= \val -> writeMaybe accessedFirst "b" >> putStrLn "read b" >> return val)
--   let !res = f (a', b')
--   -- val <- readIORef accessedFirst
--   -- case val of
--   --   (Just first) -> putStrLn (first ++ " was accessed first!")
--   --   Nothing -> putStrLn "nothing accessed??"
--   return res

data Expr a where
  SLit :: String -> Expr String
  SAppend :: Expr String -> Expr String -> Expr String
  ILit :: Int -> Expr Int
  IAdd :: Expr Int -> Expr Int -> Expr Int

interp :: Expr a -> a
interp (SLit s) = s
interp (SAppend s1 s2) = interp s1 ++ interp s2
interp (ILit i) = i
interp (IAdd i1 i2) = interp i1 + interp i2

-- test = SLit "foo" `IAdd` SLit "bar"

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

infixr 5 %:

(%:) :: x -> HList xs -> HList (x ': xs)
(%:) = HCons

instance Show (HList '[]) where
  show :: HList '[] -> String
  show = show

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
  show :: HList (x ': xs) -> String
  show (HCons x xs) = show x ++ " %: " ++ show xs

instance Eq (HList '[]) where
  (==) :: HList '[] -> HList '[] -> Bool
  (==) _ _ = True

instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs)) where
  (==) :: HList (x ': xs) -> HList (x ': xs) -> Bool
  (==) (HCons x xs) (HCons y ys) = x == y && xs == ys

class Apply f a b where
  apply :: f -> a -> b

class MapH f xs ys where
  mapH :: f -> HList xs -> HList ys

class FoldrH f acc xs where
  foldrH :: f -> acc -> HList xs -> acc

instance MapH f '[] '[] where
  mapH _ _ = HNil

instance (Apply f x' y', MapH f xs ys) => MapH f (x' : xs) (y' : ys) where
  mapH f (HCons x xs) = apply f x %: mapH f xs

instance FoldrH f acc '[] where
  foldrH :: f -> acc -> HList '[] -> acc
  foldrH _ acc _ = acc

instance (Apply f x (acc -> acc), FoldrH f acc xs) => FoldrH f acc (x : xs) where
  foldrH :: (Apply f x (acc -> acc), FoldrH f acc xs) => f -> acc -> HList (x : xs) -> acc
  foldrH f acc (HCons x xs) = apply f x (foldrH f acc xs)

data Pretty = Pretty

instance (Show x) => Apply Pretty x String where
  apply :: (Show x) => Pretty -> x -> String
  apply _ = show

data Concat = Concat

instance Apply Concat String (String -> String) where
  apply :: Concat -> String -> String -> String
  apply _ x y = x ++ ", " ++ y

type family ConstMap (t :: Type) (xs :: [Type]) :: [Type] where
  ConstMap _ '[] = '[]
  ConstMap t (x ': xs) = t ': ConstMap t xs

prettyHList ::
  forall xs.
  ( MapH Pretty xs (ConstMap String xs),
    FoldrH Concat String (ConstMap String xs)
  ) =>
  HList xs ->
  String
prettyHList hlist =
  "[" ++ foldrH Concat "" (mapH @_ @_ @(ConstMap String xs) Pretty hlist) ++ "]"

data ComparisonResult = LT | GT | EQ

newtype Compare a = Compare (a -> a -> ComparisonResult)

data Map k v = Map (Compare k) [(k, v)]

insert :: Map k v -> k -> v -> Map k v
insert map k v = undefined

lookup :: Map k v -> k -> Map k v
lookup map k = undefined

class IsBool a where
  isBool :: a -> Bool

instance IsBool a where
  isBool = const False

instance {-# OVERLAPPING #-} IsBool Bool where
  isBool = const True

-- const' :: a -> Bool
-- const' = isBool

newtype NotInt = NotInt Int

class Print a where
  print :: a -> String

instance Print NotInt where
  print (NotInt x) = if x == 0 then "zero" else "nonzero"

printNotIntDict :: Dict (Print NotInt)
printNotIntDict = Dict

unsafePrintIntDict :: Dict (Print Int)
unsafePrintIntDict = unsafeCoerce printNotIntDict

f3 :: Print a => a -> String
f3 x = case unsafePrintIntDict of
        Dict -> print x

instance Print Int where print _ = "const"
f3_Int :: Int -> String
f3_Int x = f3 x

-- fix :: (a -> a) -> a
-- fix f = fix f

class Implicit (b :: Symbol) a | b -> a where
  summon :: Implicit b a => a

instance Implicit "msg" String where
  summon = "hello world"

-- instance Implicit "msg" Int where
--   summon = 5

-- instance Implicit String where
--   summon = "hello world"

-- implicitVar :: (s :: Symbol) -> Proxy s
-- implicitVar s = undefined

-- test' :: (SSymbol "test")
-- test' = SSymbol

uhh :: Proxy a
uhh = Proxy

testMsg :: Implicit "msg" String => String
testMsg = let msg = summon @"msg" @String in "test: " ++ msg

-- instance Implicit String where
--   summon = "test"

-- testMsg :: Implicit String => String
-- testMsg = summon



{--

describeType :: (String | Int | Bool) -> String
--}

class Describe a where
  describe :: a -> String

instance Describe String where
  describe _ = "string"

instance Describe Bool where
  describe _ = "bool"

instance Describe Int where
  describe _ = "int"

describeType :: Describe a => a -> String
describeType = describe

data Describable = forall a. Describe a => Describable a

instance Describe Describable where
  describe :: Describable -> String
  describe s = describe s

test = map describe [Describable "a", Describable (1 :: Int), Describable True]

class OverloadableFunc a b where
  call :: a -> b

instance OverloadableFunc (Int, Int) Int where
  call = uncurry (+)

instance OverloadableFunc (Int, Int) Bool where
  call = uncurry (==)

instance OverloadableFunc (Int, Bool) Int where
  call = fst

class OverloadableFunc2 a b c where
  call2 :: a -> b -> c

instance OverloadableFunc2 Int Int Int where
  call2 = (+)

instance OverloadableFunc2 Int Bool Int where
  call2 = const

instance OverloadableFunc2 Int Int Bool where
  call2 = (==)

-- result :: Int -> Bool
-- -- result :: OverloadableFunc2 a Int c => a -> c
-- result x = call (x, 1 :: Int)

-- test' :: (Show a, Show b) => a -> b -> c
-- test' = undefined

-- test'' b = test' 1 b

class C a where
  find :: a

newtype Stringify a = Stringify (a -> String)

newtype Multiply a b c = Multiply (a -> b -> c)

instance C (Stringify Int) where
  find = Stringify show

instance C (Multiply Int Int Int) where
  find = Multiply (*)

stringifyList :: Stringify a -> Stringify [a]
stringifyList (Stringify f) = Stringify (intercalate ", " . map f)

instance C (Stringify a) => C (Stringify [a]) where
  -- find :: C (Stringify a) => Stringify [a]
  find = stringifyList find

stringify :: C (Stringify a) => a -> String
stringify = let (Stringify f) = find in f

times :: C (Multiply a b c) => a -> b -> c
times = let (Multiply f) = find in f

display :: C (Stringify a) => a -> IO ()
display a = do
  let s = stringify a
  putStrLn s