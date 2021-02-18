module PlayerState where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(maybeToList)
import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON)

import Common.Utils(enumAll)
import Common.Field
import Common.RNG

import Resource
import Action
import Bag
import BoardActions
import Tech


type TechId = Int

data PlayerState = PlayerState
  { _playerBag       :: Bag Resource
  , _playerAvailable :: Bag Resource
  , _playerDiscarded :: Bag Resource
  , _playerGems      :: Int
  , _playerDevel     :: Map Resource Int
  , _playerTech      :: Map TechId Tech
  , _playerRNG       :: RNG
  } deriving (Generic,ToJSON)

data CubeLoc = CubeLoc
  { cubeTech :: TechId
  , cubeAlt  :: Int
  , cubeSpot :: Int
  } deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON)

declareFields ''PlayerState




emptyPlayerState :: RNG -> PlayerState
emptyPlayerState rng =
  foldl (flip playerGainTech) s0 $ emptyPlayerBoard ++
                                   reverse (deck1 ++ deck2 ++ deck3 ++ deck4)
  where
  s0 =
   PlayerState
      { _playerBag       = bagAdd Orange
                         $ bagFromList [ r | r <- enumAll, r /= Gray ]
      , _playerAvailable = bagEmpty
      , _playerDiscarded = bagEmpty
      , _playerGems      = 0
      , _playerDevel     = Map.fromList [ (r,0) | r <- enumAll, r /= Gray ]
      , _playerTech      = Map.empty
      , _playerRNG       = rng
      }


playerGainTech :: Tech -> PlayerState -> PlayerState
playerGainTech t = updField playerTech add
  where add mp = Map.insert (Map.size mp) t mp

techAltFor :: CubeLoc -> Field PlayerState TechAlt
techAltFor loc =
  playerTech .> mapAt (cubeTech loc) .> techAlts .> listAt (cubeAlt loc)

costSpot :: CubeLoc -> Field PlayerState ResourceSpot
costSpot (CubeLoc t r i) =
  playerTech  .> mapAt t .> techAlts .> listAt r .> techCost .> listAt i


cubeToDraw :: PlayerState -> Maybe (Resource,PlayerState)
cubeToDraw p =
  do (r,rng) <- bagPick (getField playerBag p) (getField playerRNG p)
     pure (r, setField playerRNG rng p)



freeSpots :: TechId -> Tech -> [(CubeLoc,ResourceReq)]
freeSpots tid t =
  case filter hasCubes opts of
    []        -> [ loc n s r | (n,a) <- opts, (s,r) <- free a ]
    (n,a) : _ -> [ loc n s r | (s,r) <- free a ]
  where
  loc n s r = (CubeLoc { cubeTech = tid, cubeAlt = n, cubeSpot = s }, r)
  opts      = zip [0..] (getField techAlts t)
  free      = costFreeSpots . getField techCost
  hasCubes  = not . null . costFullSpots . getField techCost . snd


fullSpots :: (TechBenefit -> Bool) -> TechId -> Tech -> [(CubeLoc,Resource)]
fullSpots ok tid t =
  [ (CubeLoc { cubeTech = tid, cubeAlt = a, cubeSpot = s }, r)
  | (a,alt)  <- zip [0..] (getField techAlts t)
  , ok (techBenefit alt)
  , (s,spot) <- zip [0..] (getField techCost alt)
  , r        <- maybeToList (getField spotResource spot)
  ]

placeSpots :: PlayerState -> [CubeLoc]
placeSpots ps = [ spot
                | (techId,tech) <- Map.toList (getField playerTech ps)
                , (spot,requires) <- freeSpots techId tech
                , any (matches requires) available
                ]
  where
  available = map fst (bagToList (getField playerAvailable ps))




