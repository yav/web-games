module Common.Options
  ( Options
  , getOptLoad
  , getOptPlayers
  , getOptString
  , getOptYesNo

    -- * Defining options
  , Option
  , option
  , flag
  , optionsDefaults
  , optionsGetOpt
  ) where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(intercalate)
import Data.Maybe(fromJust)
import Data.Char(toLower)
import Data.ByteString(ByteString)
import System.Console.GetOpt

import Common.Basics(PlayerId)
import Common.Utils(enumAll)
import Common.ColorPicker(makePlayers,jsColors)


data Options = Options
  { load        :: FilePath
  , players     :: [(String,Maybe String)]
  , optHasVal   :: Map Text String
  } deriving (Read,Show)

instance Semigroup Options where
  a <> b = Options { load         = load a <> load b
                   , players      = players a <> players b
                   , optHasVal    = optHasVal a <> optHasVal b
                   }

instance Monoid Options where
  mempty = Options { load = ""
                   , players = []
                   , optHasVal = Map.empty
                   }

-- | Get the name of the save to load, if any
getOptLoad :: Options -> Maybe FilePath
getOptLoad opts = if null (load opts) then Nothing else Just (load opts)

-- | Get the player ids, and some JavaScript defining the player colors
-- The first parameter is the set of valid colors, other colors are ignored.
getOptPlayers :: [Text] -> Options -> ([PlayerId], ByteString)
getOptPlayers colors opts = (Map.keys pcs, jsColors pcs)
  where
  pcs = makePlayers colors (players opts)

getOptString :: Text -> Options -> String
getOptString x os =
  case Map.lookup x (optHasVal os) of
    Just v  -> v
    Nothing -> error ("Option `" ++ Text.unpack x ++
                                    "` is not, and has not default value.")
getOptYesNo :: Text -> Options -> Bool
getOptYesNo x opts =
  case map toLower (getOptString x opts) of
    "yes" -> True
    "no"  -> False
    _     -> error ("Option `" <> Text.unpack x <> "` requires yes/no value")


--------------------------------------------------------------------------------

data Option = Opt
  { optName    :: Text
  , optValues  :: [String]
  , optDefault :: Maybe String
  , optDescription :: String
  }


optionsDefaults :: [Option] -> Options
optionsDefaults os = mempty { optHasVal = foldr addDefault Map.empty os }
  where
  addDefault opt =
    case optDefault opt of
      Just y -> Map.insert (optName opt) y
      _      -> id


option ::
  (Show a, Bounded a, Enum a) =>
  Text -> Maybe a -> String -> (Option, Options -> a)
option nm dflt descr =
  ( Opt { optName = nm
        , optValues = vals
        , optDefault = show <$> dflt
        , optDescription = descr
        }
  , \o -> let v = lower (getOptString nm o)
          in case Map.lookup v valMap of
              Just r -> r
              Nothing -> error (err v)
  )

  where
  a = fromJust dflt
  lower = map toLower
  valMap = Map.fromList [ (lower (show x), x) | x <- enumAll `asTypeOf` [a] ]
  vals  = Map.keys valMap
  err v = unlines [ show v <> " is not a valid value for `"
                                        <> Text.unpack nm <> "`"
                  , "Valid value: " <> intercalate "|" vals
                  ]

flag :: Text -> Maybe Bool -> String -> (Option, Options -> Bool)
flag nm dflt descr =
  ( Opt { optName = nm
        , optValues = ["yes","no"]
        , optDefault = (\x -> if x then "yes" else "no") <$> dflt
        , optDescription = descr
        }
  , getOptYesNo nm
  )


--------------------------------------------------------------------------------

optionsGetOpt :: [Option] -> [ OptDescr Options ]
optionsGetOpt os = map opt os ++ common

  where

  opt x =
   Option [] [Text.unpack (optName x)]
   (ReqArg (\v -> mempty { optHasVal = Map.singleton (optName x) v })
           (intercalate "|" (optValues x)))
   (optDescription x)

  common =
    [ Option [] ["load"]
      (ReqArg (\x -> mempty { load =  x}) "FILE")
      "Load save game"

    , Option [] ["player"]
      (ReqArg playerOpt "NAME:COLOR")
      "Add a player"
    ]

  playerOpt b =
    case break (== ':') b of
      (a,_:c) -> mempty { players = [(a,Just c)] }
      (a,[])  -> mempty { players = [(a,Nothing)] }



