module PlayerState where

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(maybeToList,isJust)
import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON,
                    ToJSONKey(..),genericToJSONKey,defaultJSONKeyOptions)

import Common.Basics
import Common.Utils(enumAll)
import Common.Field
import Common.RNG

import Resource
import Action
import Bag
import BoardActions
import Tech
import RuinToken


type TechId = Int

data PlayerState = PlayerState
  { _playerBag       :: Map BagName (Bag Resource)
  , _playerGems      :: Int
  , _playerGhosts    :: Int
  , _playerWorkers   :: Int
  , _playerCaptured  :: Set PlayerId
  , _playerToken     :: [Token]
  , _playerDevel     :: Map Resource Int
  , _playerTech      :: Map TechId Tech
  , _playerRNG       :: RNG
  } deriving (Generic,ToJSON)

data BagName = BagSource | BagReady | BagDiscard
  deriving (Eq,Ord,Enum,Bounded,Generic,ToJSON)

data CubeLoc = CubeLoc
  { cubeTech :: TechId
  , cubeAlt  :: Int
  , cubeSpot :: Int
  } deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON)

instance ToJSONKey BagName where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

declareFields ''PlayerState


emptyPlayerState :: RNG -> PlayerState
emptyPlayerState rng =
  foldl (flip playerGainTech) s0 (emptyPlayerBoard
                                  ++ reverse (filter dbg (deck1 ++ deck2 ++ deck3 ++ deck4)))
  where
  dbg t = any dbgP (getField techAlts t)
  dbgP a = case techBenefit a of
             OneTime (If _ _) -> True
             _ -> False

  s0 =
   PlayerState
      { _playerBag       = Map.fromList [ (b,bagEmpty) | b <- enumAll ]
      , _playerGems      = 0
      , _playerGhosts    = 0
      , _playerCaptured  = Set.empty
      , _playerWorkers   = 10
      , _playerToken     = take 1 bronzeTokens -- XXX: Nothing
      , _playerDevel     = Map.fromList [ (r,0) | r <- enumAll, r /= Gray ]
      , _playerTech      = Map.empty
      , _playerRNG       = rng
      }


playerGainTech :: Tech -> PlayerState -> PlayerState
playerGainTech t = updField playerTech add
  where add mp = Map.insert (Map.size mp) t mp

playerNextTechId :: PlayerState -> TechId
playerNextTechId = Map.size . getField playerTech

techAltFor :: CubeLoc -> Field PlayerState TechAlt
techAltFor loc =
  playerTech .> mapAt (cubeTech loc) .> techAlts .> listAt (cubeAlt loc)

costSpot :: CubeLoc -> Field PlayerState ResourceSpot
costSpot (CubeLoc t r i) =
  playerTech  .> mapAt t .> techAlts .> listAt r .> techCost .> listAt i


cubeToDraw :: PlayerState -> Maybe (Resource,PlayerState)
cubeToDraw p =
  do (r,rng) <- bagPick (getField (playerBag .> mapAt BagSource) p)
                        (getField playerRNG p)
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
  available = map fst (bagToList (getField (playerBag .> mapAt BagReady) ps))

-- | Valid return spots on tech
returnSpots :: PlayerState -> [CubeLoc]
returnSpots = removeReturnSpots techReturnSpots

removeSpots :: ResourceReq -> PlayerState -> [CubeLoc]
removeSpots req = removeReturnSpots (techRemoveSpots req)

removeReturnSpots :: (Tech -> [(Int,Int)]) -> PlayerState -> [CubeLoc]
removeReturnSpots checkTech ps =
  [ CubeLoc { cubeTech = techId, cubeAlt = altId, cubeSpot = spotId }
  | (techId,tech) <- Map.toList (getField playerTech ps)
  , (altId,spotId) <- checkTech tech
  ]




continuousBenefits :: PlayerState -> [ContinuousAciton]
continuousBenefits player =
  [ c
  | tech <- Map.elems (getField playerTech player)
  , alt  <- getField techAlts tech
  , Continuous c <- [ techBenefit alt ]
  , and [ isJust (getField spotResource spot) | spot <- getField techCost alt ]
  ]


hideTokens :: PlayerState -> PlayerState
hideTokens = updField playerToken (fmap hideToken)
