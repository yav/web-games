module Basics where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON,
                    ToJSONKey(..),genericToJSONKey,defaultJSONKeyOptions)

newtype TeamId = TeamId Int
  deriving (Generic,ToJSON,FromJSON,Eq,Ord)

instance ToJSONKey TeamId where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions



data Card = Plague | Build | Chariots | Cycle | Drought | Flood | Miracle
  deriving (Generic,ToJSON,FromJSON,Eq,Ord,Bounded,Enum)

allCards :: [Card]
allCards = [ minBound .. maxBound ]

cardStrength :: Card -> Int
cardStrength c =
  case c of
    Plague    -> 1
    Build     -> 0
    Chariots  -> 3
    Cycle     -> 0
    Drought   -> 1
    Flood     -> 0
    Miracle   -> 0

data God = Amun | Osiris | Anubis | Isis | Ra
  deriving (Generic,ToJSON,FromJSON,Eq,Ord)

data Guardian = CatMummy | Satet | Apep | Mummy | GiantScorpion | Androsphinx
  deriving (Generic,ToJSON,FromJSON,Eq,Ord)

data GuardianSize = Small | Large
  deriving (Generic,ToJSON,FromJSON,Eq,Ord)

guardianLevel :: Guardian -> Int
guardianLevel g =
  case g of
    CatMummy       -> 1
    Satet          -> 1
    Apep           -> 2
    Mummy          -> 2
    GiantScorpion  -> 3
    Androsphinx    -> 3

guardianSize :: Guardian -> GuardianSize
guardianSize g =
  case g of
    CatMummy      -> Small
    Satet         -> Small
    Apep          -> Large
    Mummy         -> Small
    GiantScorpion -> Large
    Androsphinx   -> Large

data Monument = Obelisk | Temple | Pyramid
  deriving (Generic,ToJSON,FromJSON,Eq,Ord)

data Upgrade = Commanding | Insipring | Omnipresent | Revered
             | Resplendent | ObeliskAttuned | PyramidAttuned | TempleAttuned
             | Glorious | Magnanimous | Bountiful | Worshipful
  deriving (Generic,ToJSON,FromJSON,Eq,Ord,Enum,Bounded)

allUpgrades :: [Upgrade]
allUpgrades = [ minBound .. maxBound ]

upgradesOfLevel :: Int -> [Upgrade]
upgradesOfLevel n = [ u | u <- allUpgrades, upgradeLevel u == n ]

upgradeLevel :: Upgrade -> Int
upgradeLevel u =
  case u of
    Commanding      -> 1
    Insipring       -> 1
    Omnipresent     -> 1
    Revered         -> 1

    Resplendent     -> 2
    ObeliskAttuned  -> 2
    PyramidAttuned  -> 2
    TempleAttuned   -> 2

    Glorious        -> 3
    Magnanimous     -> 3
    Bountiful       -> 3
    Worshipful      -> 3

-- Order is important
data Action = Move | Deploy | GainFollower | Upgrade
  deriving (Generic,ToJSON,FromJSON,Eq,Ord,Enum,Bounded)

instance ToJSONKey Action where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

allActions :: [Action]
allActions = [ minBound .. maxBound ]

actionLimit :: Int -> Action -> Int
actionLimit n a =
  case a of
    Upgrade -> 1 + n
    _       -> 2 + n

data Event = GainControl | SplitRegion | Battle
  deriving (Generic,ToJSON,FromJSON,Eq,Ord)

eventOrder :: [Event]
eventOrder = replicate 3 GainControl ++
             [Battle, SplitRegion] ++
             replicate 2 GainControl ++
             [Battle, SplitRegion] ++
             replicate 2 GainControl ++
             [Battle, SplitRegion] ++
             replicate 2 GainControl ++
             [Battle, GainControl, Battle]

