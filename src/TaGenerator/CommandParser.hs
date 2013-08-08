module TaGenerator.CommandParser
       ( Command(..)
       , parseCommand
       ) where

import Control.Applicative
import TaGenerator.Parser
import TaGenerator.CommonParsers


data Command = Go String
             | PickUp String
             | LookAtInventory
             | LookAt String
             | LookAround
             | Quit
             deriving Show


parseCommand :: String -> Maybe Command
parseCommand s = fmap fst $ run commandParser s

commandParser :: Parser Char Command
commandParser = goCommand
                <|> pickUpCommand
                <|> lookAtInventoryCommand
                <|> lookAtCommand
                <|> lookAroundCommand
                <|> quitCommand

singleParameterCommand :: String -> Parser Char String
singleParameterCommand command = string command *> spaces *> anyNonEmptyString

goCommand :: Parser Char Command
goCommand = Go <$> singleParameterCommand "go"

pickUpCommand :: Parser Char Command
pickUpCommand = PickUp <$> singleParameterCommand "pick up"

lookAtInventoryCommand :: Parser Char Command
lookAtInventoryCommand = do
    string "look at inventory"
    anyValueParser LookAtInventory

lookAtCommand :: Parser Char Command
lookAtCommand = LookAt <$> singleParameterCommand "look at"

lookAroundCommand :: Parser Char Command
lookAroundCommand = string "look around" >> anyValueParser LookAround

quitCommand :: Parser Char Command
quitCommand = string "quit" >> anyValueParser Quit
