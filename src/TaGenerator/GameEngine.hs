module TaGenerator.GameEngine (startGameLoop) where

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


startGameLoop :: TextAdventure -> IO String -> IO ()
startGameLoop ta lineReader = do
    gs <- playTurnIO (initializeGameState ta) C.LookAround
    gameLoop gs lineReader

initializeGameState :: TextAdventure -> GameState
initializeGameState ta = gamestate [] startroom ta
  where startroom = roomOrFail $ getStartRoom ta
        roomOrFail (Just room) = room
        roomOrFail Nothing = error "Could not find starting room."

gamestate :: [Item] -> Room -> TextAdventure -> GameState
gamestate inventory currentroom ta =
    GameState $ GameData inventory currentroom ta

gameLoop :: GameState -> IO String -> IO ()
gameLoop gs lineReader = do
    command <- readCommand lineReader
    newGs <- playTurnIO gs command
    gameLoop' newGs lineReader

gameLoop' :: GameState -> IO String -> IO ()
gameLoop' GameOver _ = return ()
gameLoop' gs lineReader = gameLoop gs lineReader

readCommand :: IO String -> IO C.Command
readCommand lineReader = do
    line <- lineReader
    case C.parseCommand line of
        Just command -> return command
        Nothing -> putStrLn "Invalid command." >> readCommand lineReader

playTurnIO :: GameState -> C.Command -> IO GameState
playTurnIO gs command = do
    let (newGs, message) = playTurn gs command
    putStrLn message
    return newGs

playTurn :: GameState -> C.Command -> (GameState, String)
playTurn GameOver _ = (GameOver, "Game Over")
playTurn (GameState gamedata) command = playTurn' gamedata command

playTurn' :: GameData -> C.Command -> (GameState, String)
playTurn' gamedata command = case command of
    C.Quit -> quit
    C.LookAtInventory -> lookAtInventory gamedata
    C.Go target -> move gamedata target
    C.LookAt target -> lookAt gamedata target
    C.PickUp target -> pickUp gamedata target
    C.LookAround -> lookAround gamedata

quit :: (GameState, String)
quit = (GameOver, "Game over")

lookAtInventory :: GameData -> (GameState, String)
lookAtInventory gd@(GameData inventory _ _) =
    (GameState gd, showInventory inventory)

showInventory :: [Item] -> String
showInventory [] = "You have nothing in your inventory."
showInventory items = concat ["Inventory:\n", showItemsAsList items]

showItemsAsList :: [Item] -> String
showItemsAsList = concatMap renderItem
  where renderItem item = concat ["- " , getName item, "\n"]

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
  where getRoomRef = M.lookup direction . directionMap . getRoomDirections $ room

enterRoom :: Room -> String
enterRoom room = concat ["Entered ", getName room , ": \n", getDescription room]

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
    let ris = map ref $ getRoomItems room
        itemMap = taItems ta
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
itemHasName item name = (==) name . getName $ item

couldNotFind :: String -> String
couldNotFind name = "Couldn't find " ++ name

doActionOnItem :: ActionType -> Item -> Either String String
doActionOnItem actiontype item = do
    target <- listToEither failMessage actionList
    boolToEither (actionSuccess target) (T.unpack $ actionMessage target)
  where
    actionList = actionsFromItem item actiontype
    failMessage = unknownAction actiontype (getName item)

actionsFromItem :: Item -> ActionType -> [Action]
actionsFromItem item at = filter matcher . getItemActions $ item
  where matcher = (==) at . actionType

listToEither :: b -> [a] -> Either b a
listToEither b = maybeToEither b . listToMaybe

boolToEither :: Bool -> v -> Either v v
boolToEither True v = Right v
boolToEither False v = Left v

unknownAction :: ActionType -> String -> String
unknownAction at s = concat ["Don't know how to ", (show at), " ", s, "."]

pickUp :: GameData -> String -> (GameState, String)
pickUp gd name = doGameStateTransition gd (pickUp' name)

pickUp' :: String -> GameData -> Either String (GameData, String)
pickUp' name (GameData inventory room ta) = do
    (r, item) <- getItemFromRoom ta room name
    message <- doActionOnItem PickUp item
    let newItems = item : inventory
        newRoomItems = removeItemFromRoom r room
        newTa = removeItemFromTa r ta
    return (GameData newItems newRoomItems newTa, message)

removeItemFromRoom :: String -> Room -> Room
removeItemFromRoom item (Room name desc items directions) =
    let itemsWithoutGivenItem = filter ((/=) item . ref) items
    in Room name desc itemsWithoutGivenItem directions

removeItemFromTa :: String -> TextAdventure -> TextAdventure
removeItemFromTa item (TextAdventure sr rooms items) =
    let newRooms = M.map (removeItemFromRoom item) rooms
    in TextAdventure sr newRooms items

lookAround :: GameData -> (GameState, String)
lookAround gd = (GameState gd, concat [description, "\n\n", showRoomItems gd])
  where description = getDescription . currentRoom $ gd

showRoomItems :: GameData -> String
showRoomItems (GameData _ room ta) =
    let itemsInRoom = roomItems ta room
    in if null itemsInRoom
       then "No items in this room."
       else "Items in this room:\n" ++ showItemsAsList itemsInRoom

roomItems :: TextAdventure -> Room -> [Item]
roomItems ta room = mapMaybe (getItem ta) (getRoomItems room)
