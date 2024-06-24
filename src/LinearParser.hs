{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}

module LinearParser (ASTNode(..), PatternNode(..), ASTPos, LiteralOrName(..), Constructor, CaseBranch, ArgumentNode(..), parseAST, parseFile) where

import Control.Monad (void)
import Data.Functor ((<&>))
-- import Data.List (intercalate)
import Text.ParserCombinators.Parsec
-- import Control.Monad.Cont (ap)
-- import Debug.Trace (trace)
import Types (Type(..), tuple, getQuantifiedVars, AbstractFrac, pattern AbstractVar, pattern AbstractNum, CaptureEnv (CaptureEnvVar), pattern Unit, genQName, AbstractAddress (..), VarName (..), pattern FreshVarName)
import Data.Char (isLower)
import GHC.Unicode (isUpper)
import Debug.Trace (trace)
import Text.Parsec (parserTrace)

-- data Type
--   = TyVar String
--   | TyCon String
--   | TypeArrow [Type] Type
--   | TypeConstructor [Type] Type
--   | TypeApp Type [Type]
--   | TypeTuple [Type]
--   | TypeBlank
--   | TypeSchema [String] Type
--   | UnificationVar String
--   deriving (Eq)

-- instance Show Type where
--   show (TyVar s) = s
--   show (TyCon s) = s
--   show (TypeArrow args returnType) = "(-> " ++ unwords (map show (args ++ [returnType])) ++ ")"
--   show (TypeConstructor args returnType) = "(c-> " ++ unwords (map show (args ++ [returnType])) ++ ")"
--   show (TypeApp constructor args) = "(" ++ show constructor ++ " " ++ unwords (map show args) ++ ")"
--   show (TypeTuple types) = "(" ++ intercalate ", " (map show types) ++ ")"
--   show (TypeSchema q t) = "(Forall " ++ unwords q ++ " . " ++ show t ++ ")"
--   show TypeBlank = "#<blank>"
--   show (UnificationVar name) = "U." ++ name

type ASTPos = (SourcePos, SourcePos)

-- data Case = ConstructorCase ASTPos String [String] ASTNode | DefaultCase ASTPos [String] ASTNode

data PatternNode
  = ConstructorPatternNode ASTPos String [PatternNode]
  | WildCardPatternNode ASTPos String
  | TuplePatternNode ASTPos [PatternNode]
  | AsPatternNode ASTPos String PatternNode
  -- | MovePattern ASTPos String
  deriving (Show, Eq)

-- type VarName = String

data LiteralOrName = Name ASTPos String | StringLiteralNode ASTPos String | NumberLiteralNode ASTPos Float | UnitLiteral ASTPos
  deriving (Show, Eq)

type Constructor = (String, [Type])

type CaseBranch = (ASTPos, PatternNode, ASTNode)

data ArgumentNode = MoveArg LiteralOrName | SplitArg LiteralOrName
  deriving (Show, Eq)

{--

let x = 5
let y = x + 6

--}

{--

(Let x 5
(Let y x
(BinOp + x 6)))
--}

data ASTNode
  = LetPatternNode ASTPos PatternNode ASTNode ASTNode
  | -- | LetTuple ASTPos [String] LiteralOrName ASTNode
    LetFunctionNode ASTPos String [(String, Type)] Type ASTNode ASTNode
  | LetRecFunctionNode ASTPos String [(String, Type)] Type ASTNode ASTNode
  -- | Function ASTPos [String] ASTNode
  | ApplicationNode ASTPos LiteralOrName [ArgumentNode]
  | Parenthesis ASTPos ASTNode
  | TupleNode ASTPos [LiteralOrName]
  -- | Update ASTPos LiteralOrName [CaseBranch]
  | CaseNode ASTPos LiteralOrName [CaseBranch] -- (Maybe (ASTPos, String, ASTNode))
  | DataNode ASTPos String [String] [Constructor] ASTNode
  | -- | Unit ASTPos
    ListNode ASTPos [LiteralOrName]
  | RefNode ASTPos LiteralOrName
  | SetRefNode ASTPos LiteralOrName LiteralOrName
  -- | DerefInNode ASTPos LiteralOrName String ASTNode
  | DerefNode ASTPos LiteralOrName
  -- | PrintNode ASTPos LiteralOrName
  | AcquireNode ASTPos LiteralOrName String ASTNode
  | ParallelNode ASTPos [ASTNode]
  | MoveNode ASTPos LiteralOrName
  | SplitNode ASTPos LiteralOrName
  | MergeNode ASTPos LiteralOrName LiteralOrName
  | DropNode ASTPos LiteralOrName
  | ThunkNode ASTPos ASTNode
  | SemicolonNode ASTPos ASTNode ASTNode
  | LiteralNode ASTPos LiteralOrName
  | CaptureNode ASTPos LiteralOrName
  | TupleIndexNode ASTPos LiteralOrName LiteralOrName
  -- | TupleIndexMoveNode ASTPos LiteralOrName LiteralOrName
  | UnClosNode ASTPos LiteralOrName [LiteralOrName]
  | RebalanceNode ASTPos [LiteralOrName]
  | AbstractNode ASTPos LiteralOrName
  | UnabstractNode ASTPos LiteralOrName
  -- | GetFracNode ASTPos LiteralOrName
  -- | AssertFracNode ASTPos LiteralOrName LiteralOrName
  deriving (Show)

getLoc :: ASTNode -> ASTPos
getLoc (LetPatternNode loc _ _ _) = loc
-- getLoc (Function loc _ _) = loc
getLoc (ApplicationNode loc _ _) = loc
getLoc (Parenthesis loc _) = loc
getLoc (TupleNode loc _) = loc
getLoc (CaseNode loc _ _) = loc
getLoc (DataNode loc _ _ _ _) = loc
-- getLoc (Unit loc) = loc
getLoc (ListNode loc _) = loc
-- getLoc (TypeNode loc _ _ _) = loc
-- getLoc (Annotation loc _ _) = loc
getLoc (RefNode loc _) = loc
-- getLoc (RefNode loc _) = loc
-- getLoc (DerefMoveNode loc _) = loc
getLoc (DerefNode loc _) = loc
getLoc (SetRefNode loc _ _) = loc
-- getLoc (PrintNode loc _) = loc
getLoc (AcquireNode loc _ _ _) = loc
getLoc (ParallelNode loc _) = loc
getLoc (DropNode loc _) = loc
getLoc (MoveNode loc _) = loc
getLoc (SplitNode loc _) = loc
getLoc (MergeNode loc _ _) = loc
getLoc (ThunkNode loc _) = loc
-- getLoc (LetPattern loc _ _ _) = loc
getLoc (LetRecFunctionNode loc _ _ _ _ _) = loc
getLoc (LetFunctionNode loc _ _ _ _ _) = loc
getLoc (SemicolonNode loc _ _) = loc
getLoc (LiteralNode loc _) = loc
getLoc (CaptureNode loc _) = loc
getLoc (AbstractNode loc _) = loc
getLoc (UnabstractNode loc _) = loc
getLoc (RebalanceNode loc _) = loc
getLoc (TupleIndexNode loc _ _) = loc
getLoc (UnClosNode loc _ _) = loc

spaces1 :: Parser ()
spaces1 = void (many1 space)

getLiteralLoc :: LiteralOrName -> ASTPos
getLiteralLoc (Name loc _) = loc
getLiteralLoc (StringLiteralNode loc _) = loc
getLiteralLoc (NumberLiteralNode loc _) = loc

parseASTNodeNoSemicolon :: Parser ASTNode
parseASTNodeNoSemicolon =
  try parseParenthesis
    <|> try parseSetRef
    <|> try parseDeref
    <|> try parseRebalence
    <|> try parseUnClos
    <|> try parseAbstract
    <|> try parseUnabstract
    -- <|> try parseDerefMove
    -- <|> try parseAssertFrac
    -- <|> try parseGetFrac
    <|> try parseData
    -- <|> try parseFunction
    <|> try parseLetFunc
    <|> try parseLetPattern
    <|> try parseLetRecFunc
    <|> try parseTuple
    <|> try parseList
    <|> try parseThunk
    <|> try parseApplicationOrVar
    <|> try parseRef
    <|> try parseSplit
    <|> try parseMove
    <|> try parseMerge
    <|> try parseDrop
    -- <|> try parsePrint
    <|> try parseParallel
    <|> try parseAcquire
    <|> try parseCase
    <|> try parseCapture

parseCapture :: Parser ASTNode
parseCapture = do
  start <- getPosition
  char '@'
  lit <- parseLiteral
  end <- getPosition
  return (CaptureNode (start, end) lit)

parseASTNode :: Parser ASTNode
parseASTNode = do
  node <- parseASTNodeNoSemicolon
  try
    ( do
        char ';'
        spaces
        node2 <- parseASTNode
        let (start, _) = getLoc node
        let (_, end) = getLoc node
        return $ SemicolonNode (start, end) node node2
    )
    <|> return node

parseParenthesis :: Parser ASTNode
parseParenthesis = do
  start <- getPosition
  char '('
  spaces
  val <- parseASTNode
  spaces
  char ')'
  end <- getPosition
  return $ Parenthesis (start, end) val

parseTuple :: Parser ASTNode
parseTuple = do
  start <- getPosition
  char '('
  spaces
  vals <- sepBy (spaces >> parseLiteral) (char ',')
  spaces
  char ')'
  end <- getPosition
  if length vals == 1
    then error "A tuple cannot have a single element!"
    else return $ TupleNode (start, end) vals

symbol :: Parser Char
symbol = oneOf "_+*/^#"

specialSymbols :: Parser Char
specialSymbols = oneOf "-="

parseSpecialName :: Parser String
parseSpecialName = try parseSpecialNameEq -- <|> try parseSpecialNameMinus

parseSpecialNameEq :: Parser String
parseSpecialNameEq = string "=="

parseSpecialNameMinus :: Parser String
parseSpecialNameMinus = do
  val <- string "-"
  notFollowedBy (string ">")
  return val

parseNormalVariableName :: Parser String
parseNormalVariableName = do
  firstLetter <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol <|> specialSymbols)
  return (firstLetter : rest)

parseAbstract :: Parser ASTNode
parseAbstract = do
  start <- getPosition
  string ":abstract"
  spaces1
  ref <- parseLiteral
  end <- getPosition
  return (AbstractNode (start, end) ref)

parseUnabstract :: Parser ASTNode
parseUnabstract = do
  start <- getPosition
  string ":unabstract"
  spaces1
  ref <- parseLiteral
  end <- getPosition
  return (UnabstractNode (start, end) ref)

parseVariableName :: Parser String
parseVariableName = try parseSpecialName <|> parseNormalVariableName

parseNameLiteral :: Parser LiteralOrName
parseNameLiteral = do
  start <- getPosition
  name <- parseVariableName <|> parseConstructorName
  end <- getPosition
  return $ Name (start, end) name

parseString :: Parser String
parseString = between (char '"') (char '"') (many1 (noneOf "\""))

parseStringLiteral :: Parser LiteralOrName
parseStringLiteral = do
  startPos <- getPosition
  str <- parseString
  endPos <- getPosition
  return $ StringLiteralNode (startPos, endPos) str

parseSignedNumber :: Parser Integer
parseSignedNumber = do
  sign <- char '-'
  digits <- many1 digit
  return $ read $ sign : digits

parseUnsignedNumber :: Parser Integer
parseUnsignedNumber = do
  digits <- many1 digit
  return $ read digits

number :: Parser String
number = many1 digit

-- parseUnsignedFloat :: Parser Float
-- parseUnsignedFloat = read <$> parser
--   where
--     parser = (++) <$> number <*> option "" ((:) <$> char '.' <*> number)

parseUnsignedFloat :: Parser Float
parseUnsignedFloat = do
  digits <- many1 (digit <|> char '.')
  return $ read ('0':digits)

parseSignedFloat :: Parser Float
parseSignedFloat = do
  char '-'
  val <- parseUnsignedFloat
  return (negate val)

-- ap sign
parseFloat :: Parser Float
parseFloat = parseSignedFloat <|> parseUnsignedFloat


parseNumber :: Parser Float
parseNumber = parseFloat--parseSignedNumber <|> parseUnsignedNumber

parseWithPos :: (ASTPos -> a -> b) -> Parser a -> Parser b
parseWithPos f parser = do
  startPos <- getPosition
  a <- parser
  endPos <- getPosition
  return $ f (startPos, endPos) a

parseNumberLiteral :: Parser LiteralOrName
parseNumberLiteral = try (parseWithPos NumberLiteralNode parseNumber)

parseUnitLiteral :: Parser LiteralOrName
parseUnitLiteral = parseWithPos (flip (const UnitLiteral)) (string "()")

parseLiteral :: Parser LiteralOrName
parseLiteral = parseUnitLiteral <|> parseStringLiteral <|> parseNameLiteral <|> parseNumberLiteral

parseSplitArgumentNode :: Parser ArgumentNode
parseSplitArgumentNode = do
  arg <- parseLiteral
  return (SplitArg arg)

parseMoveArgumentNode :: Parser ArgumentNode
parseMoveArgumentNode = do
  char '@'
  arg <- parseLiteral
  return (MoveArg arg)

parseArgumentNode :: Parser ArgumentNode
parseArgumentNode = parseMoveArgumentNode <|> parseSplitArgumentNode

parseApplicationOrVar :: Parser ASTNode
parseApplicationOrVar = do
  start <- getPosition
  val <- parseLiteral
  spaces
  args <- sepEndBy parseArgumentNode spaces1
  end <- getPosition
  return $ case args of
    [] -> LiteralNode (start, end) val
    _ -> ApplicationNode (start, end) val args

parsePattern :: Parser PatternNode
parsePattern = {-- try parseMovePattern <|> --} try parseParenthesisPatternOrTuplePattern <|> try parseConstructorPattern <|> try parseAsPattern <|> try parseWildCardPattern

parseWildCardPattern :: Parser PatternNode
parseWildCardPattern = do
  start <- getPosition
  name <- parseVariableName
  end <- getPosition
  return $ WildCardPatternNode (start, end) name

parsePatternNoEndBy :: Parser PatternNode
parsePatternNoEndBy = {-- try parseMovePattern <|> --} try parseParenthesisPatternOrTuplePattern <|> try parseConstructorPatternNoEndBy <|> try parseAsPattern <|> try parseWildCardPattern

parseParenthesisPatternOrTuplePattern :: Parser PatternNode
parseParenthesisPatternOrTuplePattern = do
  start <- getPosition
  char '('
  spaces
  patterns <- sepBy (spaces >> parsePatternNoEndBy) (char ',')
  spaces
  char ')'
  end <- getPosition
  if length patterns == 1
    then return (head patterns)
    else return (TuplePatternNode (start, end) patterns)

parseConstructorName :: Parser String
parseConstructorName = do
  firstLetter <- upper -- <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return (firstLetter : rest)

parseConstructorPattern :: Parser PatternNode
parseConstructorPattern = do
  start <- getPosition
  constructorName <- parseConstructorName
  spaces1
  valPatterns <- endBy (try parsePattern) spaces1
  end <- getPosition
  return $ ConstructorPatternNode (start, end) constructorName valPatterns

parseConstructorPatternNoEndBy :: Parser PatternNode
parseConstructorPatternNoEndBy = do
  start <- getPosition
  constructorName <- parseConstructorName
  spaces1
  valPatterns <- sepBy (try parsePattern) spaces1
  end <- getPosition
  return $ ConstructorPatternNode (start, end) constructorName valPatterns

parseAsPattern :: Parser PatternNode
parseAsPattern = do
  start <- getPosition
  name <- parseVariableName
  char '&'
  pat <- parseParenthesisPatternOrTuplePattern
  end <- getPosition
  return (AsPatternNode (start, end) name pat)

-- parseMovePattern :: Parser Pattern
-- parseMovePattern = do
--   start <- getPosition
--   char '@'
--   name <- parseVariableName
--   end <- getPosition
--   return (MovePattern (start, end) name)

-- parseLineEndWeak :: Parser ()
-- parseLineEndWeak = (spaces >> string ":in" >> spaces) <|> (spaces >> char ';' >> spaces)

parseLineEnd :: Parser ()
parseLineEnd = spaces >> string ":in" >> spaces -- <|> (spaces >> char ';' >> spaces)

parseLetPattern :: Parser ASTNode
parseLetPattern = do
  start <- getPosition
  string ":let"
  spaces1
  pat <- parsePattern
  spaces1
  char '='
  spaces1
  val <- parseASTNode
  parseLineEnd
  body <- parseASTNode
  end <- getPosition
  return $ LetPatternNode (start, end) pat val body

{--

let
  id :: a -> a 
  id x = x in

let id (x : a) : a = x :in
--}

-- parseArg :: Parser (String, Type)
-- parseArg = do
--   char '('
--   argName <- parseVariableName
--   argType <- parseVariableType
--   char ')'

-- parseFuncBase constructor

parseFuncType :: Parser ([Type], Type)
parseFuncType = do
  tys <- between (char '(') (char ')') (sepBy1 parseType (spaces >> string "->" >> spaces))
  let returnTy:argTys = reverse tys
  return (reverse argTys, returnTy)

parseLetFunc :: Parser ASTNode
parseLetFunc = do
  start <- getPosition
  string ":let"
  spaces1
  funcName <- parseVariableName
  spaces1
  string "::"
  spaces1
  (argTys, returnTy) <- parseFuncType
  -- parserTrace (show (argTys, returnTy))
  newline
  spaces
  string funcName
  spaces1
  args <- endBy1 parseVariableName spaces1
  char '='
  spaces1
  funcBody <- parseASTNode
  parseLineEnd
  body <- parseASTNode
  end <- getPosition
  return $ LetFunctionNode (start, end) funcName (zip args argTys) returnTy funcBody body

parseLetRecFunc :: Parser ASTNode
parseLetRecFunc = do
  start <- getPosition
  string ":letrec"
  spaces1
  funcName <- parseVariableName
  spaces1
  string "::"
  spaces1
  (argTys, returnTy) <- parseFuncType
  newline
  spaces
  string funcName
  spaces1
  args <- endBy1 parseVariableName spaces1
  char '='
  spaces1
  funcBody <- parseASTNode
  parseLineEnd
  body <- parseASTNode
  end <- getPosition
  return $ LetRecFunctionNode (start, end) funcName (zip args argTys) returnTy funcBody body

-- parseLambdaSymbol :: Parser ()
-- parseLambdaSymbol = void (string "\\" <|> string "λ")

-- parseFunction :: Parser ASTNode
-- parseFunction = do
--   start <- getPosition
--   parseLambdaSymbol
--   spaces1
--   argNames <- endBy1 parseVariableName spaces1
--   string "->"
--   spaces1
--   body <- parseASTNode
--   end <- getPosition
--   return $ Function (start, end) argNames body

parseList :: Parser ASTNode
parseList = do
  start <- getPosition
  char '['
  vals <- sepBy (spaces >> parseLiteral) (char ',')
  char ']'
  end <- getPosition
  return $ ListNode (start, end) vals

parseRef :: Parser ASTNode
parseRef = do
  start <- getPosition
  string ":ref"
  spaces1
  val <- parseLiteral
  end <- getPosition
  return $ RefNode (start, end) val

-- parseDerefMove :: Parser ASTNode
-- parseDerefMove = do
--   start <- getPosition
--   string "!@"
--   val <- parseNameLiteral
--   end <- getPosition
--   return $ DerefMoveNode (start, end) val

parseDeref :: Parser ASTNode
parseDeref = do
  start <- getPosition
  char '!'
  val <- parseNameLiteral
  end <- getPosition
  return $ DerefNode (start, end) val

parseSetRef :: Parser ASTNode
parseSetRef = do
  start <- getPosition
  ref <- parseNameLiteral
  spaces1
  string ":="
  spaces1
  val <- parseLiteral
  end <- getPosition
  return $ SetRefNode (start, end) ref val

parseThunk :: Parser ASTNode
parseThunk = do
  start <- getPosition
  char '{'
  spaces
  body <- parseASTNode
  spaces
  char '}'
  end <- getPosition
  return $ ThunkNode (start, end) body

parseMove :: Parser ASTNode
parseMove = do
  start <- getPosition
  string ":move"
  spaces1
  val <- parseLiteral
  end <- getPosition
  return $ MoveNode (start, end) val

parseSplit :: Parser ASTNode
parseSplit = do
  start <- getPosition
  string ":split"
  spaces1
  val <- parseLiteral
  end <- getPosition
  return $ SplitNode (start, end) val

parseDrop :: Parser ASTNode
parseDrop = do
  start <- getPosition
  string ":drop"
  spaces1
  val <- parseLiteral
  end <- getPosition
  return $ DropNode (start, end) val

parseMerge :: Parser ASTNode
parseMerge = do
  start <- getPosition
  string ":merge"
  spaces1
  val1 <- parseLiteral
  spaces1
  string "<-"
  spaces1
  val2 <- parseLiteral
  end <- getPosition
  return $ MergeNode (start, end) val1 val2

-- parsePrint :: Parser ASTNode
-- parsePrint = do
--   start <- getPosition
--   string ":print"
--   spaces1
--   val <- parseLiteral
--   end <- getPosition
--   return $ PrintNode (start, end) val

parseAcquire :: Parser ASTNode
parseAcquire = do
  start <- getPosition
  string ":acquire"
  spaces1
  lock <- parseNameLiteral
  spaces1
  string ":as"
  spaces1
  lockName <- parseVariableName
  spaces1
  string ":in"
  spaces
  body <- parseASTNode
  end <- getPosition
  return $ AcquireNode (start, end) lock lockName body

-- parseParallel :: Parser ASTNode
-- parseParallel = do
--   start <- getPosition
--   string ":parallel"

parseParallelBranch :: Parser ASTNode
parseParallelBranch = between (char '{' >> spaces) (spaces >> char '}' >> spaces) parseASTNode

parseParallel :: Parser ASTNode
parseParallel = do
  start <- getPosition
  string ":parallel"
  spaces
  threads <- sepBy1 parseParallelBranch (char '|' >> spaces)
  end <- getPosition
  return $ ParallelNode (start, end) threads

parseConstructor :: Parser Constructor
parseConstructor = do
  constructorName <- parseConstructorName
  argTypes <- (spaces1 >> parseConstructorField `endBy` spaces1) <|> pure []
  return (constructorName, argTypes)

-- parseType :: Parser Type
-- parseType = parseVariableName <&> TyCon

-- parseTypeName :: Parser String
-- parseTypeName = many1 letter

parseTypeName :: Parser String
parseTypeName = do
  firstLetter <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ firstLetter : rest

parseTypeLiteral :: Parser Type
parseTypeLiteral = do
  name <- parseTypeName
  return $ if isUpper (head name) then TyCon name else TyVar (VarName name 0)

parseParenType :: Parser Type
parseParenType = do
  char '('
  spaces
  types <- sepBy (spaces >> parseType) (char ',')
  spaces
  char ')'
  case length types of
    0 -> return Unit
    1 -> return $ head types
    _ -> return $ tuple types

parseTypeOperand :: Parser Type
parseTypeOperand =
  parseParenType
    <|> parseFracType
    <|> parseArrowType
    <|> parseTypeLiteral

-- parseEnv :: 

{--

<a, .5 * ([a] a -> a -> a)>
<a, .5 * ([a] a -> a -> a)>
<a, 1 * ([a] a -> a -> a)>

--}

parseFracVar :: Parser AbstractFrac
parseFracVar = parseVariableName <&> AbstractVar . FreshVarName

-- floatToRational :: Float -> Rational
-- floatToRational x = toRational (trace (show x) x)

parseFracLiteral :: Parser AbstractFrac
parseFracLiteral = parseFloat <&> (AbstractNum . toRational)

parseFrac :: Parser AbstractFrac
parseFrac = parseFracVar <|> parseFracLiteral

data ParsedAddress = DefaultUniqueAddress | NamedUniqueAddress String | ActualAddress AbstractAddress

parseVarAddress :: Parser ParsedAddress
parseVarAddress = parseVariableName <&> ActualAddress . VarAddress . FreshVarName

parseUniqueAddress :: Parser ParsedAddress
parseUniqueAddress = string "#unique" >> (try parseUniqueAddressNamed <|> parseUniqueAddressUnamed)

-- defaultUniqueAddress :: String
-- defaultUniqueAddress = "a" -- "__ssduapduoihtfas__:("
-- defaultUniqueAddress = "__super_secret_default_unique_address_please_dont_use_or_ill_have_to_find_another_solution_:("

parseUniqueAddressUnamed :: Parser ParsedAddress
parseUniqueAddressUnamed = return DefaultUniqueAddress

parseUniqueAddressNamed :: Parser ParsedAddress
parseUniqueAddressNamed = spaces1 >> parseVariableName <&> NamedUniqueAddress

parseAddress :: Parser ParsedAddress
parseAddress = try parseUniqueAddress <|> parseVarAddress

parseFracType :: Parser Type
parseFracType = do
  char '<'
  spaces
  addr <- parseAddress
  spaces
  char ','
  spaces
  frac <- parseFrac
  spaces
  char '*'
  spaces
  ty <- parseType
  spaces
  -- parserTrace (show ty)
  char '>'
  case addr of
    (NamedUniqueAddress s) -> return $ TyExists [FreshVarName s] (TyFrac frac (VarAddress (FreshVarName s)) ty)
    DefaultUniqueAddress -> 
      let addrName = genQName (FreshVarName "addr") ty in
        return $ TyExists [addrName] (TyFrac frac (VarAddress addrName) ty)
    (ActualAddress addr) -> return $ TyFrac frac addr ty

parseEnv :: Parser CaptureEnv
parseEnv = do
  char '['
  env <- parseVariableName
  char ']'
  return (CaptureEnvVar (FreshVarName env))

parseArrowType :: Parser Type
parseArrowType = do
  env <- parseEnv
  spaces1
  -- string "->"
  -- args <- parseTypeOperands
  args <- sepBy1 parseType (spaces >> string "->" >> spaces)
  -- let quantifiedTypes = removeDups (args >>= getQuantifiedVars)
  -- trace ("qNames: " ++ show quantifiedTypes) (return ())
  -- let unQuantifiedArgs = map unQuantify args
  return $ TyArrow env (init args) (last args)
  -- return (if null quantifiedTypes
  --     then TyArrow env (init args) (last args)
  --     else TySchema quantifiedTypes (TyArrow env (init args) (last args)))
  -- trace (show arrowType) (return arrowType)
  -- where
  --   qualFold (TyVar name) acc = if isLower (head name) then name : acc else acc
  --   qualFold (TySchema qNames _) acc = qNames ++ acc
  --   qualFold _ acc = acc
    -- unQuantify (TySchema _ typ) = typ
    -- unQuantify typ = typ

removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = if x `elem` xs then removeDups xs else x : removeDups xs

parseTypeOperands :: Parser [Type]
parseTypeOperands = spaces1 >> sepEndBy1 parseTypeOperand spaces1

parseMaybeTypeApplication :: Parser Type
parseMaybeTypeApplication = do
  constructor <- parseTypeLiteral
  -- trace (show operator) pure 5
  (try parseTypeOperands >>= \case
                                [] -> return constructor
                                params -> return $ TyApp constructor params) <|> return constructor

parseConstructorField :: Parser Type
parseConstructorField =
  parseParenType
    <|> parseFracType
    <|> parseArrowType
    <|> parseTypeLiteral

parseType :: Parser Type
parseType =
  parseParenType
    <|> parseFracType
    <|> parseArrowType
    <|> parseMaybeTypeApplication

parseData :: Parser ASTNode
parseData = do
  start <- getPosition
  string ":data"
  spaces1
  typeName <- parseTypeName
  spaces1
  quantifiedTypes <- parseTypeName `endBy` spaces1
  char '='
  spaces1
  constructors <- sepBy1 (try (spaces >> parseConstructor)) (spaces >> char '|')
  parseLineEnd
  body <- parseASTNode
  end <- getPosition
  return $ DataNode (start, end) typeName quantifiedTypes constructors body

parseCaseBranch :: Parser CaseBranch
parseCaseBranch = do
  start <- getPosition
  pat <- parsePattern
  spaces
  string "->"
  spaces1
  body <- parseASTNode
  end <- getPosition
  return ((start, end), pat, body)

-- parseUpdate :: Parser ASTNode
-- parseUpdate = do
--   start <- getPosition
--   string ":update"
--   spaces1
--   target <- parseLiteral
--   spaces1
--   string ":of"
--   spaces
--   char '|'
--   spaces
--   branches <- sepBy1 (try (spaces >> parseCaseBranch)) (try (spaces >> char '|'))
--   spaces
--   string ":end"
--   end <- getPosition
--   return $ Update (start, end) target branches

parseCase :: Parser ASTNode
parseCase = do
  start <- getPosition
  string ":case"
  spaces1
  target <- parseLiteral
  spaces1
  string ":of"
  spaces
  char '|'
  spaces
  branches <- sepBy1 (try (spaces >> parseCaseBranch)) (try (spaces >> char '|'))
  spaces
  string ":end"
  end <- getPosition
  return $ CaseNode (start, end) target branches

parseTupleIndex :: Parser ASTNode
parseTupleIndex = do
  start <- getPosition
  string ":index"
  spaces1
  tuple <- parseLiteral
  spaces1
  index <- parseLiteral
  end <- getPosition
  return $ TupleIndexNode (start, end) tuple index

-- parseTupleIndexMove :: Parser ASTNode
-- parseTupleIndexMove = do
--   start <- getPosition
--   string ":index"
--   spaces1
--   char '@'
--   tuple <- parseLiteral
--   spaces1
--   index <- parseLiteral
--   end <- getPosition
--   return $ TupleIndexMoveNode (start, end) tuple index

-- parseGetFrac :: Parser ASTNode
-- parseGetFrac = do
--   start <- getPosition
--   string ":frac"
--   ref <- parseLiteral
--   end <- getPosition
--   return $ GetFracNode (start, end) ref

-- parseAssertFrac :: Parser ASTNode
-- parseAssertFrac = do
--   start <- getPosition
--   string ":assert"
--   spaces1
--   ref <- parseLiteral
--   spaces1
--   frac <- parseLiteral
--   end <- getPosition
--   return $ AssertFracNode (start, end) ref frac

parseUnClos :: Parser ASTNode
parseUnClos = do
  start <- getPosition
  string ":unclos"
  spaces1
  clos <- parseLiteral
  spaces
  string "->"
  spaces
  char '['
  vals <- sepBy parseLiteral spaces1
  spaces
  char ']'
  end <- getPosition
  return $ UnClosNode (start, end) clos vals

parseRebalence :: Parser ASTNode
parseRebalence = do
  start <- getPosition
  string ":rebalence"
  vals <- sepBy parseLiteral spaces1
  end <- getPosition
  return $ RebalanceNode (start, end) vals

parseAST :: Parser ASTNode
parseAST = do
  ast <- parseASTNode
  spaces
  eof
  return ast

parseFile :: String -> String -> Either ParseError ASTNode
parseFile fileName file = parse parseAST fileName file