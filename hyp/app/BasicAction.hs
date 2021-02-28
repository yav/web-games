module BasicAction where

import Control.Monad(replicateM_)
import Data.List(delete)

import Common.Basics
import Common.Field
import Common.Utils
import Common.Interact

import Resource
import Action
import Turn
import PlayerState
import AppTypes

doBasicAction :: PlayerId -> BasicAction -> Interact ()
doBasicAction playerId ba =
  case ba of
    Move -> todo
    Fly -> todo
    PlaceWorker -> todo
    CloneWorker -> todo
    Attack -> todo
    RangedAttack -> todo
    Fortify -> todo
    Develop ctr -> doSimple ba $ doUpgrade playerId ctr

    GainTech -> todo
    DrawResource -> todo
    ReturnResource -> todo
    SwapResource _ _ -> todo
    GainResource _ -> todo
    Spy -> todo

    -- these are auto activated so no need to remove
    LooseResource _ -> todo
    Gem -> update (ChangeGems playerId 1)
    LooseGem -> update (ChangeGems playerId (-1))
    LooseDevelop -> todo
    RemoveWorker -> todo
    Neighbours ba -> todo

    Times ba n -> replicateM_ n (doBasicAction playerId ba) -- hm

  where
  todo = pure ()


doSimple :: BasicAction -> Interact () -> Interact ()
doSimple ba m =
  do m
     t <- view (getField gameTurn)
     update (SetTurn (turnRemoveReady ba t))



doUpgrade :: PlayerId -> DevelopConstratint -> Interact ()
doUpgrade playerId ctr =
  case ctr of
    Same n      -> upgrade1 nonGray n >> pure ()
    Any         -> upgrade1 nonGray 1 >> pure ()
    Different n -> upgradeDiff (filter (/= Gray) enumAll) n

  where
  nonGray = filter (/= Gray) enumAll

  upgrade1 opts n =
    do ~(AskUpgrade r) <-
          choose playerId [ (AskUpgrade r, "Upgrade resource") | r <- opts ]
       have <- view (getField (playerState playerId .> playerDevel .> mapAt r))
       let n' = min (6 - have) n
       update (Upgrade playerId r n')
       pure r

  upgradeDiff opts n
    | not (null opts) && n > 0 =
      do r <- upgrade1 opts 1
         upgradeDiff (delete r opts) (n-1)
    | otherwise = pure ()




