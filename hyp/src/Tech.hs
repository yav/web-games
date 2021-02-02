module Tech where

import Data.Text(Text)
import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Resource
import Action


data Tech = Tech
  { techName      :: Text
  , techVP        :: Int
  , techCost      :: ResourceCost
  , techBenefit   :: TechBenefit
  } deriving (Generic,ToJSON)

techFreeSpots :: Tech -> [(Int,ResourceReq)]
techFreeSpots = costFreeSpots . techCost

techSetResource :: Int -> Maybe Resource -> Tech -> Tech
techSetResource n r t = t { techCost = costSetResource n r (techCost t) }



--------------------------------------------------------------------------------
defTech ::
  Int           {-^ VP -} ->
  Text          {-^ Name -} ->
  [ResourceReq] {- ^ Resource requirement -} ->
  TechBenefit   {- ^ Benefit -} ->
  Tech
defTech vp name cost benefit =
  Tech { techName     = name
       , techVP       = vp
       , techCost     = map emptySpot cost
       , techBenefit  = benefit
       }

data TechBenefit =
    OneTime Action
  | Continuous ContinuousAciton
    deriving (Generic,ToJSON)

deck1 :: [Tech]
deck1 =
  [ defTech 1 "Flying Giant Mounts"
    [ Exact Green, AnyNormal ]
  $ OneTime $ Action [ Fly 2 ]

  , defTech 1 "Citadels"
    [ Exact Red ]
  $ OneTime $ Action [ Fortify 2 ]

  , defTech 1 "Archery"
    [ Exact Red, Exact Green ]
  $ OneTime $ Action [ RangedAttack 1 ]

  , defTech 2 "Flying War Mounts"
    [ Exact Red, Exact Green, Exact Green ]
  $ OneTime $ Action [ Attack 1, Fly 2 ]

  , defTech 1 "Wagons"
    [ Exact Green ]
  $ OneTime $ Action [ Move 2 ]

  , defTech 2 "Beast Riding"
    [ Exact Red, Exact Red, Exact Green ]
  $ OneTime $ Action [ Attack 2, Move 1 ]

  , defTech 1 "Armored Mastodons"
    [ Exact Red, Exact Green ]
  $ OneTime $ Action [ Move 2, Fortify 2 ]

  , defTech 1 "Chariots"
    [ Exact Red, Exact Green ]
  $ OneTime $ Action [ Attack 1, Move 2 ]

  , defTech 1 "Weapons Forging"
    [ Exact Red ]
  $ OneTime $ Action [ Attack 1 ]

  , defTech 2 "Weapons Mastery"
    [ Exact Red, Exact Red, AnyNormal ]
  $ OneTime $ Action [ Attack 2 ]

  , defTech 2 "Flying Mounts"
    [ Exact Green ]
  $ OneTime $ Action [ Fly 1 ]

  , defTech 2 "Caravans"
    [ Exact Green, AnyNormal ]
  $ OneTime $ Action [ Move 3 ]

  , defTech 2 "Phalanx"
    [ Exact Red, AnyNormal ]
  $ OneTime $ Action [ Attack 1, Fortify 1 ]

  , defTech 2 "Weapons Supremacy"
    [ Exact Red, Exact Red, AnyNormal ]
  $ Continuous $ On GainAttack $ Attack 1

  , defTech 1 "Nomadism"
    [ Exact Green, Exact Green ]
  $ Continuous $ On GainMove $ Move 1

  , defTech 1 "Flying Ships"
    [ Exact Green ]
  $ Continuous UseMoveAsFly
  ]


deck2 :: [Tech]
deck2 =
  [ defTech 1 "Plundering"
    [ Exact Yellow, Exact Red, AnyNormal ]
  $ OneTime $ Action [ Neighbours $ Gem (-1) ]

  , defTech 1 "Sanctuaries"
    [ Exact Purple, Exact Yellow, Exact Orange ]
  $ OneTime $ If (RemoveWorker 1) [ Gem 1, Develop Any 4 ]

  , defTech 1 "Military conquests"
    [ Exact Purple, Exact Yellow ]
  $ OneTime $ Action [ PlaceWorker 1, Gem 1 ]

  , defTech 1 "Arts"
    [ Exact Yellow, Exact Yellow, AnyNormal ]
  $ OneTime $ Action [ Gem 2 ]

  , defTech 0 "Outpusts"
    [ Exact Purple, AnyNormal ]
  $ OneTime $ Action [ CloneWorker 1 ]

  , defTech 2 "Treasuries"
    [ Exact Yellow, AnyNormal ]
  $ OneTime $ Action [ Gem 1 ]

  , defTech 1 "Borderland Cities"
    [ Exact Purple ]
  $ OneTime $ Action [ CloneWorker 1, Neighbours (PlaceWorker 1) ]

  , defTech 1 "Marketplaces"
    [ Exact Yellow ]
  $ OneTime $ Action [ Gem 1 ]

  , defTech 2 "Trading Companies"
    [ Exact Yellow ]
  $ OneTime $ Action [ Gem 2, Neighbours (Gem 1) ]

  , defTech 1 "Temples"
    [ Exact Purple, Exact Yellow, AnyNormal ]
  $ OneTime $ If (RemoveWorker 1) [ Gem 2 ]

  , defTech 1 "Monasteries"
    [ Exact Purple, Exact Orange, AnyNormal ]
  $ OneTime $ If (RemoveWorker 1) [ GainResource AnyNormal 1, Develop Any 3 ]

  , defTech 2 "Peace Treaties"
    [ Exact Purple ]
  $ OneTime $ Action [ PlaceWorker 2, Neighbours (PlaceWorker 1) ]

  , defTech 1 "Huge Cities"
    [ Exact Purple ]
  $ OneTime $ Action [ PlaceWorker 1 ]

  , defTech 2 "Recruitment"
    [ Exact Purple, Exact Purple ]
  $ Continuous $ On GainWorker $ PlaceWorker 1

  , defTech 1 "Merchant Guilds"
    [ Exact Yellow, Exact Yellow, AnyNormal ]
  $ Continuous $ On GainGem $ Gem 1

  , defTech 1 "Colonization"
    [ Exact Purple, Exact Purple ]
  $ Continuous UseWorkerAsClone
  ]


deck3 :: [Tech]
deck3 =
  [ defTech 1 "Giant Libraries"
    [ Exact Orange ]
  $ OneTime $ Action [ Develop Different 2 ]

  , defTech 0 "Diplomacy"
    [ Exact Blue ]
  $ OneTime $ Action [ DrawResource 3, Neighbours (DrawResource 1) ]

  , defTech 1 "Infiltrations"
    [ Exact Blue, Exact Orange, AnyNormal ]
  $ OneTime $ Action [ Spy 1, DrawResource 1 ]

  , defTech 1 "Halls of Knowledge"
    [ Exact Blue, AnyNormal ]
  $ OneTime $ Action [ GainTech 1 ]

  , defTech 2 "Undercover Agents"
    [ Exact Blue, Exact Orange, AnyNormal ]
  $ OneTime $ Action [ Spy 1, Develop Any 2 ]

  , defTech 1 "Alchemy"
    [ Exact Blue ]
  $ OneTime $ Action [ DrawResource 1, SwapResource AnyNormal AnyNormal ]

  , defTech 1 "Crossbows"
    [ Exact Orange, Exact Red ]
  $ OneTime $ Action [ RangedAttack 1 ]

  , defTech 1 "War Ships"
    [ Exact Blue, Exact Orange, Exact Green ]
  $ OneTime $ Action [ RangedAttack 1, Move 1 ]

  , defTech 2 "Roads and Bridges"
    [ Exact Orange, AnyNormal ]
  $ OneTime $ Action [ Develop Any 3, Neighbours (Develop Any 1) ]

  , defTech 1 "Prosperity"
    [ Exact Orange, Exact Yellow ]
  $ OneTime $ Action [ Develop Any 2, Gem 1 ]

  , defTech 1 "Siege Engines"
    [ Exact Blue, Exact Red ]
  $ OneTime $ Action [ RangedAttack 1 ]

  , defTech 1 "Spying"
    [ Exact Blue, Exact Orange ]
  $ OneTime $ Action [ Spy 1 ]

  , defTech 2 "Architecture"
    [ Exact Orange, Exact Blue, AnyNormal ]
  $ OneTime $ Action [ DrawResource 2, Develop Any 2 ]

  , defTech 1 "Universities"
    [ Exact Orange ]
  $ Continuous $ On StartTurn $ Develop Any 1

  , defTech 1 "Expertise"
    [ Exact Blue, Exact Blue ]
  $ Continuous $ On StartTurn $ DrawResource 1

  , defTech 1 "Council of Elders"
    [ Exact Orange, Exact Orange ]
  $ Continuous $ On GainDevelop (Develop Any 1)
  ]


deck4 :: [Tech]
deck4 =
  [ defTech 1 "Declining Kingdoms"
    [ Exact Gray, Exact Gray, Exact Red ]
  $ OneTime $ Action [ Neighbours $ GainResource (Exact Gray) 1 ]

  , defTech 2 "Treachery"
    [ Exact Gray, Exact Gray, Exact Purple ]
  $ OneTime $ If (RemoveWorker 1) [ Neighbours (RemoveWorker 1) ]

  , defTech 1 "Corruption"
    [ Exact Gray, AnyNormal ]
  $ OneTime $ If (LooseResource AnyNormal 1) [ Gem 1 ]

  , defTech 2 "Knowledge Stealing"
    [ Exact Orange, Exact Gray, AnyNormal ]
  $ OneTime $ Action [ Develop Any 2, Neighbours (Develop Any (-1)) ]

  , defTech 2 "Cartography"
    [ Exact Gray, Exact Green ]
  $ OneTime $ Action [ DrawResource 1, Move 1 ]

  , defTech 1 "Havens"
    [ Exact Green, Exact Purple ]
  $ OneTime $ Action [ CloneWorker 1 ]

  , defTech 1 "Code of Laws"
    [ Exact Blue, Exact Gray, AnyNormal ]
  $ OneTime $ Action [ GainTech 1, DrawResource 1 ]

  , defTech 1 "Black Markets"
    [ Exact Gray, AnyNormal ]
  $ OneTime $ Action [ ReturnResource 1 ]

  , defTech 1 "Smugglers"
    [ Exact Gray, AnyNormal, AnyNormal ]
  $ OneTime $ Action [ DrawResource 1, ReturnResource 1 ]

  , defTech 2 "Barricades"
    [ Exact Red, Exact Gray ]
  $ OneTime $ Action [ Fortify 2 ]

  , defTech 1 "Technical Improvements"
    [ Exact Gray, Exact Orange ]
  $ OneTime $ Action [ SwapResource (Exact Gray) AnyNormal ]

  , defTech 1 "Mining"
  [ Exact Gray, Exact Gray, Exact Yellow ]
  $ OneTime $ Action [ Gem 2 ]

  , defTech 2 "Bureaucracy"
  [ Exact Gray, AnyNormal ]
  $ OneTime $ Action [ DrawResource 1 ]

  , defTech 1 "Mysticism"
  [ Exact Gray, Exact Blue ]
  $ Continuous $ On GainGem (DrawResource 1)

  , defTech 2 "Craftmanship"
  [ Exact Gray, Exact Orange ]
  $ Continuous $ On GainGem (Develop Any 1)

  , defTech 1 "Bastions"
  [ Exact Red, Exact Orange, Exact Gray ]
  $ Continuous $ On StartTurn (Fortify 2)
  ]
