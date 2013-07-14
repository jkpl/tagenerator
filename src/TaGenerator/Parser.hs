module Parser where

import System.Environment (getArgs)
import Data.Char
import qualified Data.Map as M
import Data.Functor
import Control.Monad
import Control.Applicative
import Control.Arrow (first)


-- Parser definition

type PResult a b = Maybe (b, [a])
type Runner a b = ([a] -> PResult a b)
newtype Parser a b = Parser { run :: Runner a b }


-- Parser instances

instance Functor (Parser a) where
    fmap f (Parser runner) = Parser $ fmap (first f) . runner

instance Applicative (Parser a) where
    pure = return
    (Parser a) <*> b = Parser $ \x -> bindToRunner (\f -> fmap f b) a x
    (Parser a) *> b = Parser $ \x -> bindToRunner (\_ -> b) a x
    (Parser a) <* (Parser b) = Parser $ \x -> do
        (f, xs) <- a x
        fmap ((,) f . snd) $ b xs

instance Alternative (Parser a) where
    (Parser a) <|> (Parser b) = Parser $ \x -> a x <|> b x
    empty = failParser

instance Monad (Parser a) where
    return a = Parser $ \x -> Just (a, x)
    (Parser runner) >>= f = Parser $ bindToRunner f runner
    fail _ = failParser

bindToRunner f runner items = do
    (result, restOfItems) <- runner items
    run (f result) restOfItems


-- Rudimentary parsers

failParser = Parser $ \_ -> Nothing

identityParser (x:xs) = Just (x, xs)
identityParser [] = Nothing

makeParser :: (a -> Bool) -> [a] -> PResult a a
makeParser _ [] = Nothing
makeParser predicate (x:xs) | predicate x = Just (x, xs)
                            | otherwise = Nothing

makeParserAlt :: (a -> Bool) -> b -> [a] -> PResult a b
makeParserAlt _ _ [] = Nothing
makeParserAlt predicate alt (x:xs) | predicate x = Just (alt, xs)
                                   | otherwise = Nothing

rollbackParser :: Parser a b -> Parser a b
rollbackParser (Parser runner) = Parser $ \xs -> do
    (result, _) <- runner xs
    return (result, xs)
