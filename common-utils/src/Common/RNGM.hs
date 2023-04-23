module Common.RNGM
  ( RNG
  , Gen
  , withRNG
  , withRNG_
  , runRNG
  , splitRNG
  , pickOne
  , pickWeighted
  , rollD
  , shuffle
  ) where

import Control.Monad(ap,liftM)

import Common.RNG(RNG)
import Common.RNG qualified as RNG

newtype Gen a = Gen (RNG -> (a,RNG))

instance Functor Gen where fmap = liftM

instance Applicative Gen where
  pure a = Gen \s -> (a,s)
  (<*>) = ap

instance Monad Gen where
  Gen m >>= f = Gen \s -> case m s of
                            (a,s1) ->
                               let Gen m1 = f a
                               in m1 s1

splitRNG :: Gen RNG
splitRNG = Gen RNG.splitRNG

-- | Assumes non-empty input
pickOne :: [a] -> Gen a
pickOne as = pickWeighted [ (a,1) | a <- as ]

-- | Assumes non-empty input
pickWeighted :: [(a,Int)] -> Gen a
pickWeighted = Gen . RNG.pickWeighted

rollD :: Int -> Gen Int
rollD = Gen . RNG.rollD

shuffle :: [a] -> Gen [a]
shuffle = Gen . RNG.shuffle

withRNG :: RNG -> Gen (RNG -> a) -> a
withRNG r (Gen m) =
  case m r of
    (f,r1) -> f r1

withRNG_ :: RNG -> Gen a -> a
withRNG_ r (Gen m) = fst (m r)

runRNG :: RNG -> Gen a -> (a, RNG)
runRNG g (Gen m) = m g


