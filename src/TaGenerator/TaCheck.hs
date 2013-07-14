{-# LANGUAGE FlexibleContexts #-}
module TaCheck (checkTextAdventure) where

import qualified Data.Map as M
import Control.Monad.Writer
import TaData

checkTextAdventure :: MonadWriter [String] m => TextAdventure -> m TextAdventure
checkTextAdventure ta = checkStartRoom ta >> checkRooms ta >> return ta

checkStartRoom :: MonadWriter [String] m => TextAdventure -> m Reference
checkStartRoom ta@(TextAdventure sr rs _) = checkReference rs "Start room" sr

checkRooms :: MonadWriter [String] m => TextAdventure -> m [Room]
checkRooms ta@(TextAdventure _ rm im) = mapM (checkRoom rm im) (M.elems rm)

checkRoom :: MonadWriter [String] m => RoomMap -> ItemMap -> Room -> m Room
checkRoom rm im r@(Room _ _ items directions) = do
    checkItems im items
    checkDirections rm directions
    return r

checkItems :: MonadWriter [String] m => ItemMap -> [Reference] -> m [Reference]
checkItems im items = mapM checkItem items
  where checkItem = checkReference im "Item"

checkDirections :: MonadWriter [String] m
                => RoomMap       -- ^ Map of rooms to check from
                -> DirectionMap  -- ^ Map of directions to check
                -> m [Reference] -- ^ Result
checkDirections rm (DirectionMap refmap) = mapM checkDirection (M.elems refmap)
  where checkDirection = checkReference rm "Room"

checkReference :: MonadWriter [String] m
               => M.Map String a -- ^ Map of references to anything
               -> String         -- ^ Reference type as string (used in logging)
               -> Reference      -- ^ Reference expected to find
               -> m Reference    -- ^ Result
checkReference m t r | M.member (ref r) m = return r
                     | otherwise = logNotDefined t r

logNotDefined :: MonadWriter [String] m => String -> Reference -> m Reference
logNotDefined t r = writer (r, [logmsg])
  where logmsg = t ++ " '" ++ (ref r) ++ "' was not defined."
