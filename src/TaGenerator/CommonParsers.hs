module TaGenerator.CommonParsers
       ( string
       , character
       , space
       , spacedChar
       , spaced
       , spaces
       , anyChar
       , noneOf
       ) where

import Control.Applicative
import Data.Char (isSpace)
import TaGenerator.Parser


string :: String -> Parser Char String
string = mapM character

character :: Char -> Parser Char Char
character c = Parser $ makeParser (c ==)

space :: Parser Char Char
space = Parser $ makeParser isSpace

spacedChar :: Char -> Parser Char Char
spacedChar c = spaced $ character c

spaced :: Parser Char a -> Parser Char a
spaced p = spaces *> p <* spaces

spaces :: Parser Char String
spaces = many space

anyChar :: Parser Char Char
anyChar = Parser identityParser

noneOf :: String -> Parser Char Char
noneOf s = Parser $ makeParser (`notElem` s)
