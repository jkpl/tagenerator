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

         -- Type class functions
       , getName
       , getDescription

         -- Getters
       , ref
       , startRoom
       , taRooms
       , taItems
       , directionMap
       , actionType
       , actionSuccess
       , actionMessage
       , getRoomItems
       , getRoomDirections
       , getItemActions

         -- Parsing
       , parseTextAdventure
       , taFromValueMap

         -- Utils
       , combineTa
       , getStartRoom
       , getRoom
       , getItem
       ) where

import Data.Monoid
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Text as T
import TaGenerator.DocumentParser
import TaGenerator.AstParser


type ReferenceMap = M.Map String Reference
type RoomMap = M.Map String Room
type ItemMap = M.Map String Item

newtype Reference = Reference { ref :: String } deriving (Eq, Show)
newtype DirectionMap = DirectionMap
                       { directionMap :: ReferenceMap } deriving Show

data TextAdventure = TextAdventure
                     { startRoom :: Reference
                     , taRooms :: RoomMap
                     , taItems :: ItemMap
                     } deriving Show
data Room = Room
            { roomName :: T.Text
            , roomDescription :: T.Text
            , getRoomItems :: [Reference]
            , getRoomDirections :: DirectionMap
            } deriving Show
data Item = Item
            { itemName :: T.Text
            , itemDescription :: T.Text
            , getItemActions :: [Action]
            } deriving Show
data Action = Action
              { actionType :: ActionType
              , actionSuccess :: Bool
              , actionMessage :: T.Text
              } deriving Show
data ActionType = PickUp | Look deriving Eq


class Info a where
    getName :: a -> String
    getDescription :: a -> String

class Identifier a where
    identifier :: a -> String


instance Info Room where
    getName = T.unpack . roomName
    getDescription = T.unpack . roomDescription

instance Info Item where
    getName = T.unpack . itemName
    getDescription = T.unpack . itemDescription


instance Identifier [Char] where
    identifier = show

instance Identifier Reference where
    identifier = ref


instance Show ActionType where
    show PickUp = "pick up"
    show Look = "look"


instance Monoid TextAdventure where
    mappend = combineTa
    mempty = TextAdventure (Reference "") M.empty M.empty

combineTa :: TextAdventure -> TextAdventure -> TextAdventure
combineTa ta1 ta2 =
    let sr = nonEmpty (startRoom ta1) (startRoom ta2)
        rm = M.union (taRooms ta1) (taRooms ta2)
        im = M.union (taItems ta1) (taItems ta2)
    in TextAdventure sr rm im
  where
    nonEmpty (Reference "") r2 = r2
    nonEmpty r1 _ = r1


instance FromAst Reference where
    fromAst (Variable s) = Just $ Reference s
    fromAst _ = Nothing

instance FromAst Room where
    fromAst = roomFromAst

roomFromAst :: Ast -> Maybe Room
roomFromAst (TypedBlock "room" vm) = valueMapToRoom vm
roomFromAst _ = Nothing

valueMapToRoom :: ValueMap -> Maybe Room
valueMapToRoom vm = Room
                    <$> key "name" vm
                    <*> key "description" vm
                    <*> key "items" vm
                    <*> key "directions" vm

instance FromAst DirectionMap where
    fromAst ast = DirectionMap <$> referenceMapFromAst ast

referenceMapFromAst :: Ast -> Maybe ReferenceMap
referenceMapFromAst (Block vm) = mapFromValueMap vm
referenceMapFromAst _ = Nothing

instance FromAst Action where
    fromAst = actionFromAst

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

instance FromAst Item where
    fromAst = itemFromAst

itemFromAst :: Ast -> Maybe Item
itemFromAst (TypedBlock "item" vm) = valueMapToItem vm
itemFromAst _ = Nothing

valueMapToItem :: ValueMap -> Maybe Item
valueMapToItem vm = Item
                    <$> key "name" vm
                    <*> key "description" vm
                    <*> key "actions" vm

parseTextAdventure :: String -> Maybe TextAdventure
parseTextAdventure s = parseDocument s >>= taFromValueMap

taFromValueMap :: ValueMap -> Maybe TextAdventure
taFromValueMap vm = TextAdventure
                    <$> startroom
                    <*> Just (valueMap vm)
                    <*> Just (valueMap vm)
  where startroom = key "start_room" vm <|> (Just emptyReference)

emptyReference :: Reference
emptyReference = Reference ""

getStartRoom :: TextAdventure -> Maybe Room
getStartRoom ta = identifierLookup (startRoom ta) (taRooms ta)

identifierLookup :: Identifier k => k -> M.Map String v -> Maybe v
identifierLookup k m = M.lookup (identifier k) m

getRoom :: Identifier a => TextAdventure -> a -> Maybe Room
getRoom ta i = identifierLookup i (taRooms ta)

getItem :: Identifier a => TextAdventure -> a -> Maybe Item
getItem ta i = identifierLookup i (taItems ta)
