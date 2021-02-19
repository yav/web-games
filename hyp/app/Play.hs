module Play where

import Data.Text(Text)
import Control.Monad(forM_,replicateM_)
import qualified Data.Map as Map
import Data.Maybe(isJust)

import Common.Basics
import Common.Utils(enumAll)
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
       do sequence_ [ replicateM_ 3 (doGainCube p c) | c <- enumAll ] -- XXX: test
          replicateM_ 3 (doDrawCube p)
     takeTurn

takeTurn :: Interact ()
takeTurn =
  do state <- getState
     let opts = actEndTurn state ++
                actPlaceCube state ++
                actReadyAction state ++
                actTest state
     askInputs opts

type Opts = State -> [ (WithPlayer Input, Text, Interact ()) ]


actTest :: Opts
actTest state =
  [ ( playerId :-> AskUpgrade Yellow
    , "Just testing"
    , do if getField (playerDevel .> mapAt Yellow) player == 6
            then update (ResetUpgrade playerId Yellow)
            else update (Upgrade playerId Yellow 1)
         takeTurn
    )
  ]
  where
  (playerId,player) = currentPlayer state

actPlaceCube :: Opts
actPlaceCube state =
  [ ( playerId :-> AskCubeLoc spot
    , "Place a cube"
    , do r <- askResouce spot
         update (RemoveFromReady playerId r)
         update (PlaceCube playerId spot r)
         checkGainBenefit playerId spot
         takeTurn
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
         -- XXX: next player, etc.
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


actReadyAction :: Opts
actReadyAction state =
  [ ( playerId :-> AskReadyAction b
    , "Use acton"
    , performBasicAction b n
    )
  | (b,n) <- bagToList (getField (gameTurn .> turnReady) state)
  , n > 0
  ]
  where
  (playerId,_) = currentPlayer state

performBasicAction :: BasicAction -> Int -> Interact ()
performBasicAction b n =
  -- XXX
  do turn <- getField gameTurn <$> getState
     update (SetTurn (updField turnReady (bagRemove b) turn))
     takeTurn

--------------------------------------------------------------------------------


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


doGainCube :: PlayerId -> Resource -> Interact ()
doGainCube pid r = update (AddToBag pid r)

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

checkGainBenefit :: PlayerId -> CubeLoc -> Interact ()
checkGainBenefit playerId lastCube =
  do state <- getState
     let player = getField (playerState playerId) state
         alt = getField (techAltFor lastCube) player
     case techBenefit alt of
       OneTime a
         | all (isJust . getField spotResource) (getField techCost alt) ->
           do let a' = foldr contModifyAction a (continuousBenefits player)
              update (SetTurn (turnAddAction a' (getField gameTurn state)))
       _ -> pure ()
