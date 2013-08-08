module TaGenerator.CommandParser
       ( Command(..)
       , parseCommand
       ) where

import Control.Applicative
import TaGenerator.Parser
import TaGenerator.CommonParsers


data Command = Go String
             | PickUp String
             | LookAt String
             | LookAround
             deriving Show


parseCommand :: String -> Maybe Command
parseCommand s = fmap fst $ run commandParser s

commandParser :: Parser Char Command
commandParser = goCommand
                <|> pickUpCommand
                <|> lookAtCommand
                <|> lookAroundCommand

singleParameterCommand :: String -> Parser Char String
singleParameterCommand command = string command *> spaces *> anyString

goCommand :: Parser Char Command
goCommand = Go <$> singleParameterCommand "go"

pickUpCommand :: Parser Char Command
pickUpCommand = PickUp <$> singleParameterCommand "pick up"

lookAtCommand :: Parser Char Command
lookAtCommand = LookAt <$> singleParameterCommand "look at"

lookAroundCommand :: Parser Char Command
lookAroundCommand = string "look around" >> anyValueParser LookAround
