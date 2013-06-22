module TaDocumentParser where

import Control.Applicative
import Data.Char
import Parser
import TaData
import qualified Data.Map as M


type ValueMap = M.Map String Value
type Assignment = (String, Value)

data Value = TypedBlock String ValueMap
           | Block ValueMap
           | List [Value]
           | StringLiteral String
           | Variable String
           deriving Show

data Token = Ch Char
           | Assign
           | BlockOpen
           | BlockClose
           | StringOpen
           | StringClose
           | ListOpen
           | ListClose
           | StatementEnd
           | ListSeparator
           deriving (Eq, Show)

-- Lexers

lexer :: String -> [Token]
lexer ('=':xs) = Assign : lexer xs
lexer ('{':xs) = BlockOpen : lexer xs
lexer ('}':xs) = BlockClose : lexer xs
lexer ('"':xs) = StringOpen : stringLexer xs
lexer ('[':xs) = ListOpen : lexer xs
lexer (']':xs) = ListClose : lexer xs
lexer (';':xs) = StatementEnd : lexer xs
lexer (',':xs) = ListSeparator : lexer xs
lexer (x:xs) | isSpace x = lexer xs
               | otherwise = (Ch x) : lexer xs
lexer [] = []

stringLexer :: String -> [Token]
stringLexer ('\\':x:xs) = (Ch x) : stringLexer xs
stringLexer ('"':xs) = StringClose : lexer xs
stringLexer (x:xs) = Ch x : stringLexer xs
stringLexer [] = []


-- Token predicates

isAnyChar :: Token -> Bool
isAnyChar (Ch _) = True
isAnyChar _ = False


-- Document parsers

token :: Token -> Parser Token Token
token t = Parser $ makeParser (t ==)

character :: Char -> Parser Token Char
character c = Parser $ makeParserAlt (Ch c ==) c

anyChar :: Parser Token Char
anyChar = Parser $ \x -> makeParser isAnyChar x >>= getAnyChar

anyString :: Parser Token String
anyString = some anyChar

string :: String -> Parser Token String
string = mapM character

assignment :: Parser Token Assignment
assignment = do
  identifier <- anyString
  token Assign
  value <- anyValue
  token StatementEnd
  return (identifier, value)

valueMap :: Parser Token ValueMap
valueMap = some assignment >>= \as -> return (toValueMap as)

blockOfAssignments :: Parser Token ValueMap
blockOfAssignments = do
  token BlockOpen
  as <- valueMap
  token BlockClose
  return as

anyValue :: Parser Token Value
anyValue = stringLiteral <|> typedBlock <|> block <|> list <|> variable

anyValues :: Parser Token [Value]
anyValues = some $ anyValue >>= \v -> token ListSeparator >> return v

stringLiteral :: Parser Token Value
stringLiteral = do
  token StringOpen
  s <- anyString
  token StringClose
  return $ StringLiteral s

typedBlock :: Parser Token Value
typedBlock = do
  s <- anyString
  as <- blockOfAssignments
  return $ TypedBlock s as

block :: Parser Token Value
block = blockOfAssignments >>= \as -> return (Block as)

list :: Parser Token Value
list = do
  token ListOpen
  vals <- anyValues
  token ListClose
  return $ List vals

variable :: Parser Token Value
variable = anyString >>= \s -> return (Variable s)


-- Helpers

getAnyChar :: (Token, [Token]) -> Maybe (Char, [Token])
getAnyChar ((Ch c), ts) = Just (c, ts)
getAnyChar _ = Nothing

toValueMap :: [Assignment] -> ValueMap
toValueMap = M.fromList
