module TaGenerator.GameEngine where

import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import TaGenerator.TaData
import qualified TaGenerator.CommandParser as C


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
    C.Go target -> move gamedata target
    C.LookAt target -> lookAt gamedata target
    C.PickUp target -> pickUp gamedata target
    C.LookAround -> lookAround gamedata

move :: GameData -> String -> (GameState, String)
move gd@(GameData inventory currentRoom ta) direction =
    case roomAtDirection ta currentRoom direction of
        Just nextRoom -> (gamestate inventory nextRoom ta, enterRoom nextRoom)
        Nothing -> (GameState gd, cantMove direction)

enterRoom :: Room -> String
enterRoom room = concat [ "Entered ", T.unpack (roomName room)
                        , ": \n\n", T.unpack (roomDescription room)
                        ]

cantMove :: String -> String
cantMove direction = "Can't move to " ++ direction

lookAt :: GameData -> String -> (GameState, String)
lookAt gd name = (GameState gd, lookAt' gd name)

lookAt' :: GameData -> String -> String
lookAt' (GameData _ room ta) name = getStringFromEither $ do
    (_, item) <- getItemFromRoom ta room name
    doActionOnItem Look item

getStringFromEither :: Either String String -> String
getStringFromEither (Left s) = s
getStringFromEither (Right s) = s

getItemFromRoom :: TextAdventure
                -> Room
                -> String
                -> Either String (String, Item)
getItemFromRoom ta room itemName =
    let item = itemFromRoom ta room itemName
        failMessage = couldNotFind itemName
    in maybeToEither failMessage item

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just a) = Right a
maybeToEither b Nothing = Left b

couldNotFind :: String -> String
couldNotFind itemName = "Couldn't find " ++ itemName

doActionOnItem :: ActionType -> Item -> Either String String
doActionOnItem actiontype item = do
    target <- listToEither failMessage actionList
    return (T.unpack $ actionMessage target)
  where
    actionList = actionsFromItem item actiontype
    failMessage = unknownAction actiontype (T.unpack $ itemName item)

listToEither :: b -> [a] -> Either b a
listToEither b = maybeToEither b . listToMaybe

unknownAction :: ActionType -> String -> String
unknownAction at s = concat ["Don't know how to ", (show at), " ", s, "."]

pickUp :: GameData -> String -> (GameState, String)
pickUp gd@(GameData inventory room ta) name = (GameState gd, "")

lookAround :: GameData -> (GameState, String)
lookAround gd = (GameState gd, description)
  where description = T.unpack . roomDescription . currentRoom $ gd
