module LinearCEK (Simple (..), Pattern (..), Term (..), ASTLoc(..), HasLoc(..), Branch(..), File(..), printLoc, fancyPrintLoc) where

import LinearParser (ASTPos)
import Types (Type)
import Text.Parsec (sourceLine)
import Text.Parsec.Pos (sourceColumn)

data Simple = Variable ASTLoc String | NumberLiteral ASTLoc Float | StringLiteral ASTLoc String | UnitSimple ASTLoc
  deriving (Show)

data Pattern = WildCardPattern ASTLoc String | ConstructorPattern ASTLoc String [String]
  deriving (Show)

type Constructor = (String, [Type])

data File = File {fileName :: String, body :: String} deriving (Show)

data ASTLoc = ASTLoc ASTPos File deriving (Show)

printLoc :: ASTLoc -> String
printLoc (ASTLoc (start, end) (File fileName body)) = "(" ++ show start ++ ":" ++ show end ++ ")"

fancyPrintLoc :: ASTLoc -> String
fancyPrintLoc (ASTLoc pos (File fileName body)) = fancyPrintPos body pos

-- formatPos :: ASTPos -> String
-- formatPos (start, end) = " (" ++ show start ++ ":" ++ show end ++ ") "

singleLinePos :: String -> Int -> Int -> Int -> String
singleLinePos src lineNum start end =
  let line = lines src !! min lineNum (length (lines src) - 1)
      prefix = show (lineNum + 1) ++ ">" in
    prefix ++ line ++ "\n" ++ replicate (length prefix + start) ' ' ++ replicate (end - start) '^' ++ replicate (length line - end) ' '

-- singleLinePos :: String -> Int -> Int -> Int -> String

fancyPrintPos :: String -> ASTPos -> String
fancyPrintPos source (start, end) =
  let startLine = sourceLine start
      startCol = sourceColumn start
      endLine = sourceLine end
      endCol = sourceColumn end in
        if startLine == endLine then singleLinePos source (startLine - 1) (startCol - 1) (endCol - 1)
        else unlines $ zipWith (\lineNum line -> show lineNum ++ " >" ++ line) [startLine .. endLine] (take (endLine - startLine) (drop (startLine - 1) (lines source)))
        -- else "\nError from line " ++ show startLine ++ " to line " ++ show endLine

data Branch = Branch ASTLoc Pattern Term deriving (Show)

data Term
  = Literal ASTLoc Simple
  | LetPattern ASTLoc Pattern Term Term
  | LetFunction ASTLoc String [String] [(String, Type)] Type Term Term
  | LetRecFunction ASTLoc String [String] [(String, Type)] Type Term Term
  | Case ASTLoc Simple [Branch]
  | Thunk ASTLoc [String] Term
  | UnitExpr ASTLoc
  | List ASTLoc [Simple]
  | Application ASTLoc Simple [Simple]
  | Move ASTLoc Simple
  | Split ASTLoc Simple
  | Merge ASTLoc Simple Simple
  | Drop ASTLoc Simple
  | RefExpr ASTLoc Simple
  | DerefIn ASTLoc String Simple Term
  | SetRef ASTLoc Simple Simple
  | Acquire ASTLoc Simple String Term
  | Parallel ASTLoc [Term]
  | Share ASTLoc Simple
  | Cooperate ASTLoc Simple
  | Annotation ASTLoc Type Term
  | Semicolon ASTLoc Term Term
  | DataTypeDeclaration ASTLoc String [String] [Constructor] Term
  | Rebalance ASTLoc [Simple]
  | UnClos ASTLoc [Simple] Simple
  | Abstract ASTLoc Simple
  | Unabstract ASTLoc Simple
  deriving (-- | Cast
            Show)

class HasLoc a where
  getLoc :: a -> ASTLoc

instance HasLoc Term where
  getLoc (Literal pos _) = pos
  getLoc (LetPattern pos _ _ _) = pos
  getLoc (LetFunction pos _ _ _ _ _ _) = pos
  getLoc (LetRecFunction pos _ _ _ _ _ _) = pos
  getLoc (Case pos _ _) = pos
  getLoc (Thunk pos _ _) = pos
  getLoc (UnitExpr pos) = pos
  getLoc (List pos _) = pos
  getLoc (Application pos _ _) = pos
  getLoc (Move pos _) = pos
  getLoc (Split pos _) = pos
  getLoc (Merge pos _ _) = pos
  getLoc (Drop pos _) = pos
  getLoc (RefExpr pos _) = pos
  getLoc (DerefIn pos _ _ _) = pos
  getLoc (SetRef pos _ _) = pos
  getLoc (Acquire pos _ _ _) = pos
  getLoc (Parallel pos _) = pos
  getLoc (Share pos _) = pos
  getLoc (Cooperate pos _) = pos
  getLoc (Annotation pos _ _) = pos
  getLoc (Semicolon pos _ _) = pos
  getLoc (DataTypeDeclaration pos _ _ _ _) = pos
  getLoc (Rebalance pos _) = pos
  getLoc (UnClos pos _ _) = pos
  getLoc (Abstract pos _) = pos
  getLoc (Unabstract pos _) = pos

instance HasLoc Simple where
  getLoc (Variable pos _) = pos
  getLoc (NumberLiteral pos _) = pos
  getLoc (StringLiteral pos _) = pos
  getLoc (UnitSimple pos) = pos

type Environment = [(String, Value)]

type NativeFunction = [Value] -> Value

newtype Pointer = Pointer Int

data Value
  = Number Float
  | Str String
  | TaggedStruct Float String [Value]
  | Closure Float Environment [String] Term
  | RecClosure Float String Environment [String] Term
  | BuiltInFunc Int NativeFunction
  | SharedRef Float Pointer
  | CooperativeRefVal Float Pointer
