module TaGenerator.AstParser where

import TaGenerator.DocumentParser
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Map as M


class FromAst a where
    fromAst :: Ast -> Maybe a


-- Basic instances

instance FromAst Bool where
    fromAst = boolConst

instance FromAst T.Text where
    fromAst = stringLiteral

instance FromAst a => FromAst [a] where
    fromAst = listFromAst

-- Parsers

boolConst :: Ast -> Maybe Bool
boolConst (Variable v) = strToBool v
boolConst _ = Nothing

strToBool :: String -> Maybe Bool
strToBool "yes" = Just True
strToBool "no" = Just False

stringLiteral :: Ast -> Maybe T.Text
stringLiteral (StringLiteral s) = Just $ T.pack s
stringLiteral _ = Nothing

variable :: Ast -> Maybe String
variable (Variable s) = Just s
variable _ = Nothing

listFromAst :: FromAst a => Ast -> Maybe [a]
listFromAst (List l) = mapM fromAst l

mapFromValueMap :: FromAst v => ValueMap -> Maybe (M.Map String v)
mapFromValueMap vm = M.fromList <$> mapM pairFromAst (M.toList vm)
  where pairFromAst (k,v) = (,) <$> Just k <*> fromAst v

valueMap :: FromAst v => ValueMap -> M.Map String v
valueMap = M.mapMaybe fromAst

key :: FromAst a => String -> ValueMap -> Maybe a
key k vm = M.lookup k vm >>= fromAst
