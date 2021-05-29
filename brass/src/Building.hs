-- | Static information about buildings.
module Building
  ( Building(..)
  , BuildingType(..)
  , buildingQueue

  , buildInAge
  , buildingCost
  , buildingCanDevelop

  , buildingProducer
  , buildingConsumer

  , buildingLinks
  , buildingIncome
  , buildingVP
  ) where

import Common.Bag
import Basics

-- | Concrete building schematics
data Building = Building
  { buildingType    :: BuildingType
  , buildingLevel   :: Int
  }

-- | Types of industry
data BuildingType =
    CoalMine   | IronWorks | Brewery            -- Producers
  | CottonMill | Pottery   | Manufacturer       -- Consumers
    deriving Eq

-- | All availabl buildings of the given type, in order.
buildingQueue :: BuildingType -> [Building]
buildingQueue buildingType =
  concat
  [ replicate (buildingQuantity b) b
  | buildingLevel <- [ 1 .. buildingLevels buildingType ]
  , let b = Building { .. }
  ]


buildingLevels :: BuildingType -> Int
buildingLevels ty =
  case ty of
    CoalMine      -> 4
    IronWorks     -> 4
    Brewery       -> 4
    CottonMill    -> 4
    Pottery       -> 5
    Manufacturer  -> 8

-- | How many of each building we have
buildingQuantity :: Building -> Int
buildingQuantity Building { .. } =
  case buildingType of
    CoalMine      -> table [ 1, 2, 2, 2 ]
    IronWorks     -> table [ 1, 1, 1, 1 ]
    Brewery       -> table [ 2, 2, 2, 1 ]
    Manufacturer  -> table [ 1, 2, 1, 1, 2, 1, 1, 2 ]
    CottonMill    -> table [ 3, 2, 3, 3 ]
    Pottery       -> table [ 1, 1, 1, 1, 1 ]
  where
  table xs  = xs !! (buildingLevel - 1)



-- | Production buildings are complete when all the resources on them are
-- consumed.
buildingProducer :: Age -> Building -> Maybe (Int,Resource)
buildingProducer age Building { .. } =
  case buildingType of
    CoalMine  ->
      Just (buildingLevel + 1, Coal)

    IronWorks ->
      Just (if buildingLevel == 1 then 4 else buildingLevel + 2, Iron)

    Brewery ->
      Just (if age == Canal then 1 else 2, Beer)

    _  -> Nothing

-- | How much beer is needed to sell this building
buildingConsumer :: Building -> Maybe Int
buildingConsumer Building { .. } =
  case buildingType of

    CottonMill -> Just 1

    Pottery
      | buildingLevel == 3 || buildingLevel == 5 -> Just 2
      | otherwise                                -> Just 1

    Manufacturer
      | buildingLevel == 3 || buildingLevel == 7 -> Just 0
      | buildingLevel == 5                       -> Just 2
      | otherwise                                -> Just 1

    _ -> Nothing


-- | Can this building be "developed" (i.e., discarded without building)
buildingCanDevelop :: Building -> Bool
buildingCanDevelop Building { .. } =
  case buildingType of
    Pottery -> buildingLevel /= 1 || buildingLevel /= 3
    _       -> True





-- | Number of links for edge scoring, if building is complete
buildingLinks :: Building -> Int
buildingLinks Building { .. } =
  case buildingType of
    CoalMine      -> if buildingLevel == 1 then 2 else 1

    IronWorks     -> 1

    Brewery       -> 2

    Manufacturer
      | buildingLevel == 3 || buildingLevel == 7 -> 0
      | buildingLevel == 1 || buildingLevel == 5 -> 2
      | otherwise                                -> 1

    CottonMill    -> if buildingLevel == 2 then 2 else 1

    Pottery       -> 1

-- | Income points increase when starting the building
buildingIncome :: Building -> Int
buildingIncome Building { .. } =
  case buildingType of
    CoalMine      -> table [ 4, 7, 6, 5 ]
    IronWorks     -> table [ 3, 3, 2, 1 ]
    Brewery       -> table [ 4, 5, 5, 5 ]
    Manufacturer  -> table [ 5, 1, 4, 6, 2, 6, 4, 1 ]
    CottonMill    -> table [ 5, 4, 3, 2 ]
    Pottery       -> table [ 5, 1, 5, 1 ]
  where
  table xs  = xs !! (buildingLevel - 1)

-- | VP during scoring if this building is completed
buildingVP :: Building -> Int
buildingVP Building { .. } =
  case buildingType of
    CoalMine      -> table [ 1, 2, 3, 4 ]
    IronWorks     -> table [ 3, 5, 7, 9 ]
    Brewery       -> table [ 4, 5, 7, 10 ]
    Manufacturer  -> table [ 3, 5, 4, 3, 8, 7, 9, 11 ]
    CottonMill    -> table [ 5, 5, 9, 12 ]
    Pottery       -> table [ 10, 1, 11, 1, 20 ]
  where
  table xs  = xs !! (buildingLevel - 1)

-- | The cost to build a building
buildingCost :: Building -> Cost
buildingCost Building { .. } =
   case buildingType of
    CoalMine      -> table [ pay 5 []
                           , pay 7 []
                           , pay 8 [Iron]
                           , pay 10 [Iron]
                           ]
    IronWorks     -> table (zipWith pay [5,7,9,12] (repeat [Coal]))
    Brewery       -> table (zipWith pay [5,7,9,9]  (repeat [Iron]))
    Manufacturer  -> table [ pay 8 [Coal]
                           , pay 10 [Iron]
                           , pay 12 [Coal,Coal]
                           , pay 8  [Iron]
                           , pay 16 [Coal]
                           , pay 20 []
                           , pay 16 [Coal,Iron]
                           , pay 12 [Iron,Iron]
                           ]
    CottonMill    -> table [ pay 12 []
                           , pay 14 [Coal]
                           , pay 16 [Coal,Iron]
                           , pay 18 [Coal,Iron]
                           ]
    Pottery       -> table [ pay 17 [Iron]
                           , pay 0  [Coal]
                           , pay 22 [Coal,Coal]
                           , pay 0  [Coal]
                           , pay 24 [Coal,Coal]
                           ]
  where
  table xs  = xs !! (buildingLevel - 1)
  pay x ys  = Cost { costMoney = x, costResource = bagFromList ys }

-- | Can we build this building in the given age
buildInAge :: Age -> Building -> Bool
buildInAge age Building { .. } =
  case age of

    Canal ->
      case buildingType of
        Brewery -> buildingLevel /= 4
        Pottery -> buildingLevel /= 5
        _       -> True

    Rail  -> buildingLevel /= 1 || buildingType == Pottery




