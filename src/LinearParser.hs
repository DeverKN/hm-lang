{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE PatternSynonyms #-}



module LinearParser (ASTNode(..), PatternNode(..), ASTPos, LiteralOrName(..), Constructor, CaseBranch, ArgumentNode(..), parseAST, parseFile) where

import Control.Monad (void)
import Data.Functor ((<&>))
-- import Data.List (intercalate)
import Text.ParserCombinators.Parsec
-- import Control.Monad.Cont (ap)
-- import Debug.Trace (trace)
import Types (RawType(..), AbstractFrac, pattern AbstractVar, pattern AbstractNum, CaptureEnv (CaptureEnvVar), pattern Unit, AbstractAddress (..), VarName (..), pattern FreshVarName, RawType, toRaw, rawTuple, genQNameRaw, genQNameRawList)
-- import Data.Char (isLower)
import GHC.Unicode (isUpper)
import Debug.Trace (trace, traceShow)
import Text.Parsec (parserTrace)
import Data.Either (lefts, rights)
import Data.Maybe (fromMaybe, fromJust)
-- import GHC.Generics (U1 (U1), (:*:) ((:*:)))

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

type Constructor = (String, [RawType])

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
    LetFunctionNode ASTPos String [(String, RawType)] RawType ASTNode ASTNode
  | LetRecFunctionNode ASTPos String [(String, RawType)] RawType ASTNode ASTNode
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
  | UnClosNode ASTPos LiteralOrName --[LiteralOrName]
  | RebalanceNode ASTPos [LiteralOrName]
  | AbstractNode ASTPos LiteralOrName
  | UnabstractNode ASTPos LiteralOrName
  | TypeSynonymNode ASTPos String [String] RawType ASTNode
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
getLoc (UnClosNode loc _) = loc
getLoc (TypeSynonymNode loc _ _ _ _) = loc

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
    <|> try parseTypeSynonym
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
symbol = oneOf "_+*/^"

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
  negate <$> parseUnsignedFloat

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
parseSplitArgumentNode = SplitArg <$> parseLiteral

parseMoveArgumentNode :: Parser ArgumentNode
parseMoveArgumentNode = do
  char '@'
  MoveArg <$> parseLiteral

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

parseFuncType :: Parser ([RawType], RawType)
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

parseTypeLiteral :: Parser RawType
parseTypeLiteral = do
  name <- parseTypeName
  return $ if isUpper (head name) then RawTyCon name else RawTyVar (VarName name 0)

parseParenType :: Parser RawType
parseParenType = do
  char '('
  spaces
  types <- sepBy (spaces >> parseType) (char ',')
  spaces
  char ')'
  case length types of
    0 -> return (toRaw Unit)
    1 -> return $ head types
    _ -> return $ rawTuple types


data TypeOperand = Ty RawType | UnnamedUnique | NamedUnique String deriving (Show)

parseTypeOperandTy :: Parser RawType
parseTypeOperandTy =
  parseParenType
    <|> parseFracType
    <|> parseArrowType
    <|> parseTypeLiteral

parseTypeOperandUnique :: Parser TypeOperand
parseTypeOperandUnique = parseUnique <&> maybe UnnamedUnique NamedUnique

parseTypeOperand :: Parser TypeOperand
parseTypeOperand = try parseTypeOperandUnique <|> (parseTypeOperandTy <&> Ty)

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
parseFracLiteral = parseFloat <&> AbstractNum . toRational

parseFrac :: Parser AbstractFrac
parseFrac = parseFracVar <|> parseFracLiteral

data ParsedAddress = DefaultUniqueAddress | NamedUniqueAddress String | ActualAddress AbstractAddress

parseVarAddress :: Parser ParsedAddress
parseVarAddress = parseVariableName <&> ActualAddress . VarAddress . FreshVarName

parseUnique :: Parser (Maybe String)
parseUnique = void (string "#unique") >> (try parseUniqueNamed <|> parseUniqueUnamed)

-- parseUnique :: Parser ()
-- parseUnique = void (string "#unique")

parseUniqueUnamed :: Parser (Maybe String)
parseUniqueUnamed = return Nothing

parseUniqueNamed :: Parser (Maybe String)
parseUniqueNamed = char '-' >> parseVariableName >>= \name -> if name == "addr" then error "Cannot use \"addr\" as the name for a named #unique" else return $ Just name

parseUniqueAddress :: Parser ParsedAddress
parseUniqueAddress = parseUnique <&> maybe DefaultUniqueAddress NamedUniqueAddress

-- defaultUniqueAddress :: String
-- defaultUniqueAddress = "a" -- "__ssduapduoihtfas__:("
-- defaultUniqueAddress = "__super_secret_default_unique_address_please_dont_use_or_ill_have_to_find_another_solution_:("



parseAddress :: Parser ParsedAddress
parseAddress = try parseUniqueAddress <|> parseVarAddress

parseFracType :: Parser RawType
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
    (NamedUniqueAddress s) ->
      let addrName = genQNameRaw (FreshVarName s) ty in
        return $ RawTyExists [addrName] (RawTyFrac frac (VarAddress (FreshVarName s)) ty)
    DefaultUniqueAddress ->
      let addrName = genQNameRaw (FreshVarName "addr") ty in
        return $ RawTyExists [addrName] (RawTyFrac frac (VarAddress addrName) ty)
    (ActualAddress addr) -> return $ RawTyFrac frac addr ty

parseEnv :: Parser CaptureEnv
parseEnv = do
  char '['
  env <- parseVariableName
  char ']'
  return (CaptureEnvVar (FreshVarName env))

parseArrowType :: Parser RawType
parseArrowType = do
  env <- parseEnv
  spaces1
  -- string "->"
  -- args <- parseTypeOperands
  args <- sepBy1 parseType (spaces >> string "->" >> spaces)
  -- let quantifiedTypes = removeDups (args >>= getQuantifiedVars)
  -- trace ("qNames: " ++ show quantifiedTypes) (return ())
  -- let unQuantifiedArgs = map unQuantify args
  return $ RawTyArrow env (init args) (last args)
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

parseTypeOperands :: Parser [TypeOperand]
parseTypeOperands = spaces1 >> sepEndBy1 parseTypeOperand spaces1

-- isUnique :: TypeOperand -> Bool
-- isUnique (Ty _) = False
-- isUnique Unique = True

renameUnique :: String -> VarName -> Either String RawType -> Either String RawType
renameUnique old new (Left s) | s == old = Right (RawTyVar new)
renameUnique _ _ ty = ty

maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just

allRight :: [Either a b] -> Maybe [b]
allRight = mapM maybeRight

getUniqueNames :: [TypeOperand] -> ([VarName], [RawType])
getUniqueNames = phaseTwo . phaseOne
  where phaseTwo :: ([String], [Either String RawType]) -> ([VarName], [RawType])
        phaseTwo (uniqueNames, tys) =
          let rawTys = rights tys
              gennedUniqueNames = map (\name -> (name, genQNameRawList (FreshVarName name) rawTys)) uniqueNames in
              let renamedTys = foldr (\(old, new) tys -> map (renameUnique old new) tys) tys gennedUniqueNames in
                  (map snd gennedUniqueNames, fromJust (allRight renamedTys))
        phaseOne :: [TypeOperand] -> ([String], [Either String RawType])
        phaseOne = foldr (\op (names, tys) ->
                          case op of
                            Ty ty -> (names, Right ty:tys)
                            UnnamedUnique -> ("addr":names, Left "addr":tys)
                            NamedUnique name -> (name:names, Left name:tys)) ([], [])

handleRawTyApp :: RawType -> [TypeOperand] -> RawType
handleRawTyApp constructor [] = constructor
handleRawTyApp constructor params =
  case getUniqueNames params of
    ([], tys) -> RawTyApp constructor tys
    (names, tys) -> RawTyExists names (RawTyApp constructor tys)

parseMaybeTypeApplication :: Parser RawType
parseMaybeTypeApplication = do
  constructor <- parseTypeLiteral
  -- trace (show operator) pure 5
  (try parseTypeOperands <&> handleRawTyApp constructor) <|> return constructor

parseConstructorField :: Parser RawType
parseConstructorField =
  parseParenType
    <|> parseFracType
    <|> parseArrowType
    <|> parseTypeLiteral

parseType :: Parser RawType
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

parseTypeSynonym :: Parser ASTNode
parseTypeSynonym = do
  start <- getPosition
  string ":type"
  spaces1
  typeName <- parseTypeName
  spaces1
  quantifiedTypes <- parseTypeName `endBy` spaces1
  char '='
  spaces1
  ty <- parseType
  parseLineEnd
  body <- parseASTNode
  end <- getPosition
  return $ TypeSynonymNode (start, end) typeName quantifiedTypes ty body

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
  -- spaces
  -- string "->"
  -- spaces
  -- char '['
  -- vals <- sepBy parseLiteral spaces1
  -- spaces
  -- char ']'
  end <- getPosition
  return $ UnClosNode (start, end) clos

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
parseFile = parse parseAST