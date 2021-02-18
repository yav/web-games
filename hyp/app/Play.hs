module Play where

import Data.Text(Text)
import Control.Monad(forM_,replicateM_)
import qualified Data.Map as Map

import Common.Basics
import Common.Field
import Common.Interact

import Bag
import Turn
import AppTypes
import Action
import PlayerState
import Resource

setup :: Interact ()
setup =
  do state <- getState
     forM_ (gameTurnOrder state) \p ->
       replicateM_ 3 (doDrawCube p)
     takeTurn

takeTurn :: Interact ()
takeTurn =
  do state <- getState
     let opts = actEndTurn state ++
                actPlaceCube state
     askInputs opts

endTurn :: Interact ()
endTurn = takeTurn

type Opts = State -> [ (WithPlayer Input, Text, Interact ()) ]

actPlaceCube :: Opts
actPlaceCube state =
  [ ( playerId :-> AskCubeLoc spot
    , "Place a cube"
    , do r <- askResouce spot
         update (RemoveFromReady playerId r)
         update (PlaceCube playerId spot r)
         endTurn
    ) | spot <- placeSpots player
  ]
  where
  (playerId,player) = currentPlayer state

  askResouce spot =
     case spotRequires (getField (costSpot spot) player) of
       Exact r -> pure r
       AnyNormal ->
         case opts of
           [r] -> pure r
           _   -> do ~(AskReady r) <- choose playerId
                                [ (AskReady r, "Place on wild") | r <- opts ]
                     pure r
         where
         avail = getField playerAvailable player
         opts  = filter (/= Gray) (map fst (bagToList avail))

actEndTurn :: Opts
actEndTurn state =
  [ ( playerId :-> AskButton "End Turn"
    , "End Turn"
    , do discardReady
         drawNew
         takeTurn
    )
  ]
  where
  (playerId,player) = currentPlayer state
  discardReady =
    forM_ (bagToList (getField playerAvailable player)) \(r,n) ->
      replicateM_ n
      do update (RemoveFromReady playerId r)
         update (AddToDiscard playerId r)
  drawNew =
    do have <- doDrawCube playerId
       if have
          then replicateM_ 2 (doDrawCube playerId)
          else doReset playerId




doReset :: PlayerId -> Interact ()
doReset playerId =
  do player <- getField (playerState playerId) <$> getState
     forM_ (bagToList (getField playerDiscarded player)) \(r,n) ->
        replicateM_ n
        do update (RemoveFromDiscard playerId r)
           update (AddToBag playerId r)
     let ok b = case b of
                  Continuous {} -> False
                  OneTime {}          -> True
     forM_ (Map.toList (getField playerTech player)) \(tid,t) ->
       forM_ (fullSpots ok tid t) \(spot,r) ->
         do update (RemoveCube playerId spot)
            update (AddToBag playerId r)
     -- XXX: ask for cont.

     replicateM_ 3 (doDrawCube playerId)


doDrawCube :: PlayerId -> Interact Bool
doDrawCube playerId =
  do state <- getState
     let player = getField (playerState playerId) state
     case cubeToDraw player of
       Nothing -> pure False
       Just (r,p1) ->
         do localUpdate_ (setField (playerState playerId) p1)
            update (RemoveFromBag playerId r)
            update (AddToReady playerId r)
            pure True
