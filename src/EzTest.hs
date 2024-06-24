{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module EzTest (missing, assert, TestResult (..), handleTests, handleIOTests) where

import Control.Monad (unless)
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack (SrcLoc (..), callStack, getCallStack, popCallStack)
import GHC.Stack.Types (HasCallStack)

-- import Directory (renameFile)

class JMEq a b where
  jmeq :: a -> b -> Bool

class Inspect a where
  inspect :: a -> String

instance {-# OVERLAPS #-} Inspect String where
  inspect :: String -> String
  inspect s = "\"" ++ s ++ "\""

instance (Show a) => Inspect a where
  inspect :: a -> String
  inspect = show

instance (Eq a) => JMEq a a where
  jmeq :: a -> a -> Bool
  jmeq a1 a2 = a1 == a2

instance JMEq a NewTestResult where
  jmeq :: a -> NewTestResult -> Bool
  jmeq _ _ = error "Can't compare heterogenous types (this type is lying to you!)"

class Assertable a where
  loc :: a -> Maybe SrcLoc
  loc _ = Nothing

newtype NewTestResult = NewTestResult SrcLoc

instance {-# OVERLAPPING #-} Assertable NewTestResult where
  loc :: NewTestResult -> Maybe SrcLoc
  loc (NewTestResult loc) = Just loc

instance Show NewTestResult where
  show _ = "uhh..."

instance Eq NewTestResult where
  (==) :: NewTestResult -> NewTestResult -> Bool
  (==) _ _ = False

instance Assertable a

-- {-# INLINE getLineNum #-}
-- getLineNum :: HasCallStack => SrcLoc
-- getLineNum = snd (head (getCallStack callStack))

-- getCallStack' :: [([Char], SrcLoc)]
-- getCallStack' = getCallStack callStack

getCallerLineNum :: HasCallStack => SrcLoc
getCallerLineNum = snd (head (getCallStack (popCallStack callStack)))

-- getCallerFuncLineNum :: HasCallStack => SrcLoc
-- getCallerFuncLineNum = snd (head (getCallStack (popCallStack (popCallStack callStack))))

-- srcLocToSource :: SrcLoc -> IO String
-- srcLocToSource loc = do
--   let path = srcLocFile loc
--   file <- readFile path
--   return $ lookupLocInFile (lines file) loc

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- lookupLocInFile :: [String] -> SrcLoc -> String
-- lookupLocInFile fileLines loc =
--   let startLine = srcLocStartLine loc - 1
--       startCol = srcLocStartCol loc
--       endLine = srcLocEndLine loc - 1
--       endCol = srcLocEndCol loc
--    in if startLine == endLine
--         then slice (startCol - 1) (endCol - 2) (fileLines !! startLine)
--         else
--           let firstLine = slice startCol (length $ fileLines !! startLine) (fileLines !! startLine)
--               lastLine = slice 0 endCol (fileLines !! endLine)
--               innerLines = slice (startLine + 1) (endLine - 1) fileLines
--            in unlines $ [firstLine] ++ innerLines ++ [lastLine]

dropLast :: Int -> [a] -> [a]
dropLast n xs = reverse (drop n (reverse xs))

lookupLocInFile' :: String -> Int -> Int -> Int -> Int -> String
lookupLocInFile' file startLine' startCol' endLine' endCol' =
  let fileLines = lines file
      -- term = file !! (length file - 1)
      startLine = startLine' - 1
      startCol = startCol' - 1
      endLine = endLine' - 1
      endCol = endCol' - 1
   in if startLine == endLine
        then slice startCol endCol (fileLines !! startLine)
        else
          let firstLine = slice startCol (length $ fileLines !! startLine) (fileLines !! startLine)
              lastLine = slice 0 endCol (fileLines !! endLine)
              innerLines = slice (startLine + 1) (endLine - 1) fileLines
           in dropLast 1 (unlines $ [firstLine] ++ innerLines ++ [lastLine])

updateLocInFile' :: String -> Int -> Int -> Int -> Int -> String -> String
updateLocInFile' file startLine' startCol' endLine' endCol' new =
  let fileLines = lines file
      startLine = startLine' - 1
      startCol = startCol' - 1
      endLine = endLine' - 1
      endCol = endCol' - 1
   in if startLine == endLine
        then
          unlines $
            take startLine fileLines
              ++ [take startCol (fileLines !! startLine) ++ new ++ drop endCol (fileLines !! startLine)]
              ++ drop (startLine + 1) fileLines
        else
          dropLast
            1
            ( unlines $
                take startLine fileLines
                  ++ [take startCol (fileLines !! startLine) ++ new ++ drop endCol (fileLines !! endLine)]
                  ++ drop (endLine + 1) fileLines
            )

-- weirdFunc :: (HasCallStack) => () -> String
-- weirdFunc () = "called at: " ++ prettySrcLoc getCallerLineNum

-- line <- getLineNum
-- print (prettySrcLoc line)
-- print (callStack)
-- return $ freezeCallStack callStack

-- assert :: HasCallStack => String -> Bool -> IO String
-- assert _ True = return "Passed"
-- assert msg False = do
--   src <- srcLocToSource getCallerLineNum
--   return $ "Assertion: " ++ msg ++ " failed at: " ++ src

-- test :: HasCallStack => String
-- test = assert "test" (1 == 2)

missing :: (HasCallStack) => NewTestResult
missing = NewTestResult getCallerLineNum

-- showLoc :: SrcLoc -> String
-- showLoc (SrcLoc _ _ _ stLn stCol endLn endCol) = show stLn ++ show stCol ++ " to " ++ show endLn ++ show endCol

-- assert :: HasCallStack => a -> SrcLoc -> String
-- assert _ loc = "from " ++ showLoc getCallerLineNum ++ " to: " ++ showLoc loc

-- getLeadingSpaces

-- trim :: String -> String
-- trim (' ':s) = trim s
-- trim s = s

generateSuggestedTest :: (Inspect a) => String -> a -> String
generateSuggestedTest src val = dropLast (length "missing") src ++ "(" ++ inspect val ++ ")"

-- instance Show NoQuotes where show (NoQuotes str) = str
-- replaceStrInFile :: FilePath -> FilePath -> T.Text -> T.Text -> IO ()
-- replaceStrInFile infileName outfileName needle replacement = T.readFile infileName >>= \txt -> T.writeFile outfileName (T.replace needle replacement txt)

assert :: (Inspect a, Inspect b, Assertable b, JMEq a b, HasCallStack) => String -> a -> b -> TestResult
-- assert _ True = return "Passed"
assert msg val (loc -> (Just missingLoc)) = TestCreated msg getCallerLineNum missingLoc val
assert msg val expectedVal =
  if jmeq val expectedVal
    then TestPassed msg
    else TestFailed msg val expectedVal

-- input <- getLine
-- if input == "y"
--   then undefined

-- assertEq :: Eq a => String -> a -> a -> ()
-- assertEq = undefined

data TestResult = forall a b. (Inspect a, Inspect b, JMEq a b) => TestFailed String a b | TestPassed String | forall a. (Inspect a) => TestCreated String SrcLoc SrcLoc a

-- data ForAll = forall a. ForAll a

-- woah :: ForAll -> ForAll
-- woah (ForAll a) = (ForAll 2)

data TestResultTy = Passed | Failed | New

handleTestResult :: TestResult -> IO TestResultTy
handleTestResult (TestPassed msg) = putStrLn (msg ++ " passed!") >> return Passed
handleTestResult (TestFailed msg actualVal expectedVal) = putStrLn (msg ++ " failed :(\n⍟ expected: " ++ inspect expectedVal ++ " but got: " ++ inspect actualVal) >> return Failed
handleTestResult (TestCreated _msg assertLoc missingLoc val) = do
  let path = srcLocFile assertLoc
  file <- T.readFile path <&> T.unpack
  let startLine = srcLocStartLine assertLoc
  let startCol = srcLocStartCol assertLoc
  let endLine = srcLocEndLine missingLoc
  let endCol = srcLocEndCol missingLoc
  let src = lookupLocInFile' file startLine startCol endLine endCol
  let suggestion = generateSuggestedTest src val
  putStrLn $ "Actual result: " ++ inspect val
  putStrLn $ "Suggestion: " ++ suggestion ++ "\nAccept? [y]/n"
  input <- getLine
  unless
    (input == "n")
    ( T.readFile path
        >>= ( \file ->
                T.writeFile
                  path
                  ( T.pack
                      ( updateLocInFile'
                          file
                          startLine
                          startCol
                          endLine
                          endCol
                          suggestion
                      )
                  )
                  >> putStrLn "Updated!"
            )
          . T.unpack
    )
  return New

countResult :: TestResultTy -> (Int, Int, Int)
countResult Passed = (1, 0, 0)
countResult Failed = (0, 1, 0)
countResult New = (0, 0, 1)

binThruple :: (a -> a -> a) -> (a, a, a) -> (a, a, a) -> (a, a, a)
binThruple f (x1, y1, z1) (x2, y2, z2) = (f x1 x2, f y1 y2, f z1 z2)

-- removeMe :: HasCallStack => IO ()
-- removeMe = do
--   let loc = getCallerLineNum
--   let path = srcLocFile loc
--   file <- T.readFile path <&> T.unpack
--   let startLine = srcLocStartLine loc
--   let startCol = srcLocStartCol loc
--   let endLine = srcLocEndLine loc
--   let endCol = srcLocEndCol loc
--   T.writeFile path (T.pack (updateLocInFile' file startLine startCol endLine endCol ""))
--   putStrLn "poof!"

doList :: Monad m => [m a] -> m [a]
doList [] = return []
doList (x:xs) = do
  x <- x
  xs <- doList xs
  return $ x:xs

handleIOTests :: [IO TestResult] -> IO ()
handleIOTests tests = do
  tests <- doList tests
  handleTests tests

handleTests :: [TestResult] -> IO ()
handleTests tests = do
  results <- mapM handleTestResult tests
  let resultsCount = foldr (binThruple (+) . countResult) (0, 0, 0) results
  case resultsCount of
    (passed, 0, 0) -> putStrLn $ "All " ++ show passed ++ " tests passed!!"
    (passed, failed, new) -> putStrLn $ show passed ++ " tests passed, " ++ show failed ++ " tests failed, " ++ show new ++ " new tests"