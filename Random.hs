module Random where

import System.Random
import Data.Random.Normal

import Num
import Helpers

randomBoolWithP :: (RandomGen g) => P -> g -> (Bool, g)
randomBoolWithP p g = applyFst (p >) $ randomR (0.0, 1.0) g

randomBool :: (RandomGen g) => g -> (Bool, g)
randomBool = randomBoolWithP 0.5

callWithP :: (RandomGen g) => P -> (v -> g -> (v, g)) -> v -> g -> (v, g)
callWithP p f v g = if doit then (v, g1) else f v g1
  where (doit, g1) = randomBoolWithP p g

