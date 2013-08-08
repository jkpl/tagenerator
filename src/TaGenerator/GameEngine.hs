module TaGenerator.GameEngine where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
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
gamestate inventory currentroom ta = GameState $ GameData inventory currentroom ta

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
move gd direction = doGameStateTransition gd (move' direction)

doGameStateTransition :: GameData
                      -> (GameData -> Either String (GameData, String))
                      -> (GameState, String)
doGameStateTransition gd f = case f gd of
    Left s -> (GameState gd, s)
    Right (newgd, s) -> (GameState newgd, s)

move' :: String -> GameData -> Either String (GameData, String)
move' direction (GameData inventory room ta) = do
    nextRoom <- roomAtDirection ta room direction
    return (GameData inventory nextRoom ta, enterRoom nextRoom)

roomAtDirection :: TextAdventure -> Room -> String -> Either String Room
roomAtDirection ta room direction =
    maybeToEither (cantMove direction) (roomAtDirection' ta room direction)

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just a) = Right a
maybeToEither b Nothing = Left b

roomAtDirection' :: TextAdventure -> Room -> String -> Maybe Room
roomAtDirection' ta room direction = getRoomRef >>= getRoom ta
    where getRoomRef = M.lookup direction . directionMap . directions $ room

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
getItemFromRoom ta room name =
    let item = itemFromRoom ta room name
        failMessage = couldNotFind name
    in maybeToEither failMessage item

itemFromRoom :: TextAdventure -> Room -> String -> Maybe (String, Item)
itemFromRoom ta room name =
    let ris = map ref $ roomItems room
        itemMap = items ta
        itemsInRoom = M.filterWithKey (hasKeyInList ris) itemMap
    in getFirstFromMap . filterItems $ itemsInRoom
  where
    hasKeyInList list k _ = k `elem` list
    filterItems itemMap = filterItemMapWithName itemMap name

getFirstFromMap :: M.Map k v -> Maybe (k, v)
getFirstFromMap m = listToMaybe $ M.toList m

filterItemMapWithName :: ItemMap -> String -> ItemMap
filterItemMapWithName itemMap name = M.filter matcher itemMap
  where matcher item = itemHasName item name

itemHasName :: Item -> String -> Bool
itemHasName item name = (==) name . T.unpack . itemName $ item

couldNotFind :: String -> String
couldNotFind name = "Couldn't find " ++ name

doActionOnItem :: ActionType -> Item -> Either String String
doActionOnItem actiontype item = do
    target <- listToEither failMessage actionList
    return (T.unpack $ actionMessage target)
  where
    actionList = actionsFromItem item actiontype
    failMessage = unknownAction actiontype (T.unpack $ itemName item)

actionsFromItem :: Item -> ActionType -> [Action]
actionsFromItem item at = filter matcher . itemActions $ item
  where matcher = (==) at . actionType

listToEither :: b -> [a] -> Either b a
listToEither b = maybeToEither b . listToMaybe

unknownAction :: ActionType -> String -> String
unknownAction at s = concat ["Don't know how to ", (show at), " ", s, "."]

pickUp :: GameData -> String -> (GameState, String)
pickUp gd@(GameData inventory room ta) name = (GameState gd, "")

lookAround :: GameData -> (GameState, String)
lookAround gd = (GameState gd, concat [description, "\n\n", showRoomItems gd])
  where description = T.unpack . roomDescription . currentRoom $ gd

showRoomItems :: GameData -> String
showRoomItems (GameData _ room ta) =
    let itemsInRoom = getRoomItems ta room
    in if null itemsInRoom
       then "No items in this room."
       else "Items in this room:\n" ++ showItemsAsList itemsInRoom

showItemsAsList :: [Item] -> String
showItemsAsList = concatMap renderItem
  where renderItem item = concat ["- " , (T.unpack $ itemName item) , "\n"]

getRoomItems :: TextAdventure -> Room -> [Item]
getRoomItems ta room = mapMaybe (getItem ta) (roomItems room)
