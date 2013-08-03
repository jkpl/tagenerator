module TaGenerator.DocumentParser
       ( parseDocument
       , ValueMap
       , Assignment
       , Ast(..)
       ) where

import Control.Applicative
import Data.Char (isSpace)
import TaGenerator.Parser
import TaGenerator.CommonParsers
import qualified Data.Map as M


type ValueMap = M.Map String Ast
type Assignment = (String, Ast)

data Ast = TypedBlock String ValueMap
         | Block ValueMap
         | List [Ast]
         | StringLiteral String
         | Variable String
         deriving Show


parseDocument :: String -> Maybe ValueMap
parseDocument = fmap fst . run valueMap

anyWord :: Parser Char String
anyWord = some . Parser $ makeParser isWordChar

isWordChar :: Char -> Bool
isWordChar c = not $ isSpecialChar c || isSpace c

isSpecialChar :: Char -> Bool
isSpecialChar c = c `elem` specialChars

specialChars :: String
specialChars = "{}[]=\";"

escapedString :: Parser Char String
escapedString = many escapedChar

escapedChar :: Parser Char Char
escapedChar = (escapeChar >> anyChar) <|> noneOf "\""

escapeChar :: Parser Char Char
escapeChar = Parser $ makeParser ('\\' ==)

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

toValueMap :: [Assignment] -> ValueMap
toValueMap = M.fromList

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
