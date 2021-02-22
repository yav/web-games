module BasicAction where

import Control.Monad(replicateM_)

import Common.Basics
import Common.Interact

import Action
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
    Develop ctr -> todo
    GainTech -> todo
    DrawResource -> todo
    ReturnResource -> todo
    SwapResource _ _ -> todo
    GainResource _ -> todo
    Spy -> todo

    LooseResource _ -> todo
    Gem -> update (ChangeGems playerId 1)
    LooseGem -> update (ChangeGems playerId (-1))
    LooseDevelop -> todo
    RemoveWorker -> todo

    Neighbours ba -> todo

    Times ba n -> replicateM_ n (doBasicAction playerId ba) -- hm

  where
  todo = pure ()
