module TaGenerator.DocumentParser
       ( parseDocument
       , ValueMap
       , Assignment
       , Ast(..)
       ) where

import Control.Applicative
import Data.Char
import TaGenerator.Parser
import qualified Data.Map as M


type ValueMap = M.Map String Ast
type Assignment = (String, Ast)

data Ast = TypedBlock String ValueMap
         | Block ValueMap
         | List [Ast]
         | StringLiteral String
         | Variable String
         deriving Show


-- Publics

parseDocument :: String -> Maybe ValueMap
parseDocument = fmap fst . run valueMap


-- Predicates

specialChars :: String
specialChars = "{}[]=\";"

isSpecialChar :: Char -> Bool
isSpecialChar c = c `elem` specialChars

isWordChar :: Char -> Bool
isWordChar c = not $ isSpecialChar c || isSpace c


-- Basic character parsers

character :: Char -> Parser Char Char
character c = Parser $ makeParser (c ==)

string :: String -> Parser Char String
string = mapM character

anyWord :: Parser Char String
anyWord = some . Parser $ makeParser isWordChar

space :: Parser Char Char
space = Parser $ makeParser isSpace

spaces :: Parser Char String
spaces = many space

spaced :: Parser Char a -> Parser Char a
spaced p = spaces *> p <* spaces

spacedChar :: Char -> Parser Char Char
spacedChar c = spaced $ character c

noneOf :: String -> Parser Char Char
noneOf s = Parser $ makeParser (`notElem` s)

escapeChar :: Parser Char Char
escapeChar = Parser $ makeParser ('\\' ==)

anyChar :: Parser Char Char
anyChar = Parser identityParser

escapedChar :: Parser Char Char
escapedChar = (escapeChar >> anyChar) <|> noneOf "\""

escapedString :: Parser Char String
escapedString = many escapedChar


-- Document parsers

stringLiteral :: Parser Char Ast
stringLiteral = StringLiteral <$> sl
  where sl = character '"' *> escapedString <* character '"'

assignment :: Parser Char Assignment
assignment = do
    identifier <- anyWord
    spacedChar '='
    value <- anyValue
    spacedChar ';'
    return (identifier, value)

valueMap :: Parser Char ValueMap
valueMap = toValueMap <$> some assignment

blockOfAssignments :: Parser Char ValueMap
blockOfAssignments = spacedChar '{' *> valueMap <* spacedChar '}'

anyValue :: Parser Char Ast
anyValue = stringLiteral
           <|> typedBlock
           <|> block
           <|> list
           <|> variable

anyValues :: Parser Char [Ast]
anyValues = many $ anyValue <* spaces

typedBlock :: Parser Char Ast
typedBlock = TypedBlock <$> spaced anyWord <*> blockOfAssignments

block :: Parser Char Ast
block = Block <$> blockOfAssignments

list :: Parser Char Ast
list = List <$> (spacedChar '[' *> anyValues <* spacedChar ']')

variable :: Parser Char Ast
variable = Variable <$> anyWord


-- Helpers

toValueMap :: [Assignment] -> ValueMap
toValueMap = M.fromList
