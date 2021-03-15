module Common.RNG where

import Data.Aeson(ToJSON(..))
import qualified Data.Aeson as JS
import Data.Word(Word64)
import Control.Monad.ST
import System.Random.TF(newTFGen,mkSeedUnix,seedTFGen)
import System.Random.TF.Gen(TFGen,split)
import System.Random.TF.Instances(randomR)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

newtype RNG = RNG TFGen

instance ToJSON RNG where
  toJSON _ = JS.object []

newRNG :: IO RNG
newRNG = RNG <$> newTFGen

type Seed = (Word64,Word64,Word64,Word64)

newRNGSeed :: IO Seed
newRNGSeed = mkSeedUnix

seedRNG :: Seed -> RNG
seedRNG = RNG . seedTFGen


splitRNG :: RNG -> (RNG,RNG)
splitRNG (RNG x) = (RNG a, RNG b)
  where (a,b) = split x

-- | Assumes non-empty input
pickWeighted :: [(a,Int)] -> RNG -> (a, RNG)
pickWeighted xs (RNG rng) = (head $ drop i $ concatMap expand xs, RNG rng1)
  where
  (i,rng1) = randomR (0,tot-1) rng
  tot = sum (map snd xs)
  expand (a,n) = replicate n a

shuffle :: [a] -> RNG -> ([a],RNG)
shuffle xs (RNG g0)
  | null xs = ([],RNG g0)
  | otherwise =
    runST do v <- Vector.unsafeThaw (Vector.fromList xs)
             mutShuffle v g0 (MVector.length v - 1)
  where
  mutShuffle v g i
    | i > 0 =
      case randomR (0,i) g of
        (j,g1) -> do swap v i j
                     mutShuffle v g1 (i-1)

    | otherwise = do v1 <- Vector.unsafeFreeze v
                     pure (Vector.toList v1, RNG g)

  swap v a b =
    do x <- MVector.read v a
       y <- MVector.read v b
       MVector.write v a y
       MVector.write v b x

