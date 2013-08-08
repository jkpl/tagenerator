{-# LANGUAGE FlexibleContexts #-}
module TaGenerator.TaCheck (checkTextAdventure, runChecker) where

import qualified Data.Map as M
import Control.Monad.Writer
import TaGenerator.TaData


runChecker :: TextAdventure -> [String]
runChecker = snd . runWriter . checkTextAdventure

checkTextAdventure :: MonadWriter [String] m
                   => TextAdventure   -- ^ TextAdventure to check
                   -> m TextAdventure -- ^ Check results
checkTextAdventure ta = checkStartRoom ta >> checkRooms ta >> return ta

checkStartRoom :: MonadWriter [String] m => TextAdventure -> m Reference
checkStartRoom ta@(TextAdventure sr rs _) = checkReference rs "Start room" sr

checkRooms :: MonadWriter [String] m => TextAdventure -> m [Room]
checkRooms ta@(TextAdventure _ roommap itemmap) =
    mapM (checkRoom roommap itemmap) (M.elems roommap)

checkRoom :: MonadWriter [String] m
          => RoomMap            -- ^ Map of rooms to look other rooms from
          -> ItemMap            -- ^ Map of items to look items from
          -> Room               -- ^ Room to check
          -> m Room             -- ^ Check results
checkRoom rm im r@(Room _ _ items directions) = do
    checkItems im items
    checkDirections rm directions
    return r

checkItems :: MonadWriter [String] m
           => ItemMap           -- ^ Map of items to look items from
           -> [Reference]       -- ^ Item references to check
           -> m [Reference]     -- ^ Check results
checkItems itemmap items = mapM checkItem items
  where checkItem = checkReference itemmap "Item"

checkDirections :: MonadWriter [String] m
                => RoomMap       -- ^ Map of rooms to check from
                -> DirectionMap  -- ^ Map of directions to check
                -> m [Reference] -- ^ Check result
checkDirections roommap (DirectionMap refmap) =
    mapM checkDirection (M.elems refmap)
  where checkDirection = checkReference roommap "Room"

checkReference :: MonadWriter [String] m
               => M.Map String a -- ^ Map of references to anything
               -> String         -- ^ Reference type (used in logging)
               -> Reference      -- ^ Reference expected to find
               -> m Reference    -- ^ Check result
checkReference m t r | M.member (ref r) m = return r
                     | otherwise = logNotDefined t r

logNotDefined :: MonadWriter [String] m => String -> Reference -> m Reference
logNotDefined t r = writer (r, [logmsg])
  where logmsg = t ++ " '" ++ (ref r) ++ "' was not defined."
