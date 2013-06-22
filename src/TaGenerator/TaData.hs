module TaData where


data TextAdventure = TextAdventure { startRoom :: Room
                                   , rooms :: [Room]
                                   , items :: [Item]
                                      } deriving Show
data Room = Room { roomName :: String
                 , roomDescription :: String
                 , roomItems :: [Item]
                 , directions :: [Direction]
                 } deriving Show
data Direction = Direction String Room deriving Show
data Item = Item { itemName :: String
                 , itemDescription :: String
                 , itemActions :: [Action]
                 } deriving Show
data Action = Action { actionType :: ActionType
                     , actionSuccess :: Bool
                     , actionMessage :: String
                     } deriving Show
data ActionType = PickUp | Look deriving Show
