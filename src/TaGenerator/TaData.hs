{-# LANGUAGE FlexibleInstances #-}
module TaGenerator.TaData
       ( ReferenceMap
       , RoomMap
       , ItemMap
       , Reference(Reference)
       , DirectionMap(DirectionMap)
       , TextAdventure(TextAdventure)
       , Room(Room)
       , Item(Item)
       , Action(Action)
       , ActionType(..)

         -- Getters
       , ref
       , startRoom
       , rooms
       , items
       , roomName
       , roomDescription
       , roomItems
       , directions
       , directionMap
       , itemName
       , itemDescription
       , itemActions
       , actionType
       , actionSuccess
       , actionMessage

         -- Parsing
       , parseTextAdventure
       , taFromValueMap

         -- Utils
       , combineTa
       , getStartRoom
       , getRoom
       , getItem
       , roomAtDirection
       , itemFromRoom
       , actionFromItem
       ) where

import Data.Maybe (listToMaybe)
import Data.Monoid
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Text as T
import TaGenerator.DocumentParser
import TaGenerator.AstParser


type ReferenceMap = M.Map String Reference
type RoomMap = M.Map String Room
type ItemMap = M.Map String Item

newtype Reference = Reference { ref :: String } deriving Show
newtype DirectionMap = DirectionMap
                       { directionMap :: ReferenceMap } deriving Show

data TextAdventure = TextAdventure
                     { startRoom :: Reference
                     , rooms :: RoomMap
                     , items :: ItemMap
                     } deriving Show
data Room = Room
            { roomName :: T.Text
            , roomDescription :: T.Text
            , roomItems :: [Reference]
            , directions :: DirectionMap
            } deriving Show
data Item = Item
            { itemName :: T.Text
            , itemDescription :: T.Text
            , itemActions :: [Action]
            } deriving Show
data Action = Action
              { actionType :: ActionType
              , actionSuccess :: Bool
              , actionMessage :: T.Text
              } deriving Show
data ActionType = PickUp | Look deriving Eq


class Identifier a where
    identifier :: a -> String

instance Identifier [Char] where
    identifier = show

instance Identifier Reference where
    identifier = ref

instance Show ActionType where
    show PickUp = "pick up"
    show Look = "look"


-- Useful instances and functions

instance Monoid TextAdventure where
    mappend = combineTa
    mempty = TextAdventure (Reference "") M.empty M.empty

combineTa :: TextAdventure -> TextAdventure -> TextAdventure
combineTa ta1 ta2 =
    let sr = nonEmpty (startRoom ta1) (startRoom ta2)
        rm = M.union (rooms ta1) (rooms ta2)
        im = M.union (items ta1) (items ta2)
    in TextAdventure sr rm im
  where
    nonEmpty (Reference "") r2 = r2
    nonEmpty r1 _ = r1

getStartRoom :: TextAdventure -> Maybe Room
getStartRoom ta = identifierLookup (startRoom ta) (rooms ta)

getRoom :: Identifier a => TextAdventure -> a -> Maybe Room
getRoom ta i = identifierLookup i (rooms ta)

getItem :: Identifier a => TextAdventure -> a -> Maybe Item
getItem ta i = identifierLookup i (items ta)

identifierLookup :: Identifier k => k -> M.Map String v -> Maybe v
identifierLookup k m = M.lookup (identifier k) m

roomAtDirection :: TextAdventure -> Room -> String -> Maybe Room
roomAtDirection ta room dir = getRoomRef >>= getRoom ta
    where getRoomRef = M.lookup dir . directionMap . directions $ room

itemFromRoom :: TextAdventure -> Room -> String -> Maybe (String, Item)
itemFromRoom ta room itemName =
    let ris = map ref $ roomItems room
    in firstFromMap $ M.filterWithKey (\k _ -> k `elem` ris) (items ta)

filterItemsWithName :: ItemMap -> String -> ItemMap
filterItemsWithName items name = M.filter matcher items
  where matcher = (==) name . T.unpack . itemName

firstFromMap :: M.Map k v -> Maybe (k, v)
firstFromMap = listToMaybe . M.toList

actionFromItem :: Item -> ActionType -> Maybe Action
actionFromItem item at = listToMaybe . filter matcher . itemActions $ item
  where matcher = (==) at . actionType


-- Parse

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
