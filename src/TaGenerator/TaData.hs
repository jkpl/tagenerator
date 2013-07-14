module TaData
  ( ReferenceMap
  , RoomMap
  , ItemMap
  , Reference(Reference)
  , DirectionMap(DirectionMap)
  , TextAdventure(TextAdventure)
  , Room(Room)
  , Item(Item)
  , Action(Action)
  , ActionType(PickUp, Look)
  , ref
  , startRoom
  , rooms
  , items
  , roomName
  , roomDescription
  , roomItems
  , directions
  , itemDescription
  , itemActions
  , actionType
  , actionSuccess
  , actionMessage
  , parseTextAdventure
  , taFromValueMap
  ) where

import Control.Applicative
import DocumentParser
import AstParser
import qualified Data.Map as M
import qualified Data.Text as T


type ReferenceMap = M.Map String Reference
type RoomMap = M.Map String Room
type ItemMap = M.Map String Item
newtype Reference = Reference { ref :: String } deriving Show
newtype DirectionMap = DirectionMap ReferenceMap deriving Show

data TextAdventure = TextAdventure { startRoom :: Reference
                                   , rooms :: RoomMap
                                   , items :: ItemMap
                                   } deriving Show
data Room = Room { roomName :: T.Text
                 , roomDescription :: T.Text
                 , roomItems :: [Reference]
                 , directions :: DirectionMap
                 } deriving Show
data Item = Item { itemName :: T.Text
                 , itemDescription :: T.Text
                 , itemActions :: [Action]
                 } deriving Show
data Action = Action { actionType :: ActionType
                     , actionSuccess :: Bool
                     , actionMessage :: T.Text
                     } deriving Show
data ActionType = PickUp | Look deriving Show


-- Publics

parseTextAdventure :: String -> Maybe TextAdventure
parseTextAdventure s = parseDocument s >>= taFromValueMap


-- From AST to TextAdventure

instance FromAst Reference where
  fromAst (Variable s) = Just $ Reference s
  fromAst _ = Nothing

instance FromAst Room where
  fromAst = roomFromAst

instance FromAst DirectionMap where
  fromAst ast = DirectionMap <$> referenceMapFromAst ast

instance FromAst Action where
  fromAst = actionFromAst

instance FromAst Item where
  fromAst = itemFromAst


taFromValueMap :: ValueMap -> Maybe TextAdventure
taFromValueMap vm = TextAdventure
                    <$> key "start_room" vm
                    <*> Just (valueMap vm)
                    <*> Just (valueMap vm)

referenceMapFromAst :: Ast -> Maybe ReferenceMap
referenceMapFromAst (Block vm) = mapFromValueMap vm
referenceMapFromAst _ = Nothing

roomFromAst :: Ast -> Maybe Room
roomFromAst (TypedBlock "room" vm) = valueMapToRoom vm
roomFromAst _ = Nothing

valueMapToRoom :: ValueMap -> Maybe Room
valueMapToRoom vm = Room
                    <$> key "name" vm
                    <*> key "description" vm
                    <*> key "items" vm
                    <*> key "directions" vm

actionFromAst :: Ast -> Maybe Action
actionFromAst (TypedBlock t vm) = Action
                                  <$> strToActionType t
                                  <*> key "success" vm
                                  <*> key "message" vm
actionFromAst _ = Nothing

strToActionType :: String -> Maybe ActionType
strToActionType "pickup" = Just PickUp
strToActionType "look" = Just Look
strToActionType _ = Nothing

itemFromAst :: Ast -> Maybe Item
itemFromAst (TypedBlock "item" vm) = valueMapToItem vm
itemFromAst _ = Nothing

valueMapToItem :: ValueMap -> Maybe Item
valueMapToItem vm = Item
                    <$> key "name" vm
                    <*> key "description" vm
                    <*> key "actions" vm
