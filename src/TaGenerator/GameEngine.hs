module TaGenerator.GameEngine where

import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import TaGenerator.TaData
import qualified TaGenerator.CommandParser as C


type ActionResult = (Bool, String)
data GameState = GameOver | GameState GameData
data GameData = GameData
                 { carrying :: [Item]
                 , currentRoom :: Room
                 , textAdventure :: TextAdventure
                 } deriving Show


initializeGameState :: TextAdventure -> GameState
initializeGameState ta = gamestate [] startroom ta
  where startroom = roomOrFail $ getStartRoom ta
        roomOrFail (Just room) = room
        roomOrFail Nothing = error "Could not find starting room."

gamestate :: [Item] -> Room -> TextAdventure -> GameState
gamestate items currentroom ta = GameState $ GameData items currentroom ta

interact :: GameState -> C.Command -> (GameState, String)
interact GameOver _ = (GameOver, "Game Over")
interact (GameState gamedata) command = interact' gamedata command

interact' :: GameData -> C.Command -> (GameState, String)
interact' gamedata command = case command of
    (C.Go target) -> move gamedata target
    (C.LookAt target) -> lookAt gamedata target
    (C.PickUp target) -> pickUp gamedata target
    C.LookAround -> lookAround gamedata

move :: GameData -> String -> (GameState, String)
move gd@(GameData inventory currentRoom ta) direction =
    case roomAtDirection ta currentRoom direction of
        Just nextRoom -> (gamestate inventory nextRoom ta, enterRoom nextRoom)
        Nothing -> (GameState gd, cantMove direction)

lookAt :: GameData -> String -> (GameState, String)
lookAt gd@(GameData inventory room ta) name = (GameState gd, "")

pickUp :: GameData -> String -> (GameState, String)
pickUp gd@(GameData inventory room ta) name = (GameState gd, "")

lookAround :: GameData -> (GameState, String)
lookAround gd = (GameState gd, description)
  where description = T.unpack . roomDescription . currentRoom $ gd

doActionOnItem :: ActionType -> Item -> ActionResult
doActionOnItem actiontype item = doForFirst action success failMessage
  where action = actionsFromItem item actiontype
        success a = (actionSuccess a, T.unpack $ actionMessage a)
        failMessage = unknownAction actiontype (T.unpack $ itemName item)

doForFirst :: [a] -> (a -> (Bool, String)) -> String -> (Bool, String)
doForFirst targets f failMessage =
    case listToMaybe targets of
        Just firstTarget -> f firstTarget
        Nothing -> (False, failMessage)

unknownAction :: ActionType -> String -> String
unknownAction at s = concat ["Don't know how to ", (show at), " ", s, "."]

enterRoom :: Room -> String
enterRoom room = concat [ "Entered ", T.unpack (roomName room)
                        , ": \n\n", T.unpack (roomDescription room)]

cantMove :: String -> String
cantMove direction = "Can't move to " ++ direction
