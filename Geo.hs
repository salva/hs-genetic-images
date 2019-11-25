module Geo where

import Data.Complex
import System.Random
import Data.Random.Normal

import Num
import Random

randomCInUnitSquare :: (RandomGen g) => g -> (C, g)
randomCInUnitSquare g = ((x :+ y), g2)
  where (x, g1) = randomR (0.0, 1.0) g
        (y, g2) = randomR (0.0, 1.0) g1

randomVersor :: (RandomGen g) => g -> (C, g)
randomVersor g =
  let (x, g1) = normal g
      (y, g2) = normal g1
      d2 = x * x + y * y
  in if d2 == 0.0
     then (1.0 :+ 0.0, g2)
     else let dInv = 1.0 / (sqrt d2)
          in ((x * dInv :+ y * dInv), g2)

mutateRWithP :: (RandomGen g) => P -> SD -> R -> g -> (R, g)
mutateRWithP p sd r g = callWithP p mutate r g
  where mutate r g = normal' (r, sd) g

mutateCWithP :: (RandomGen g) => P -> SD -> C -> g -> (C, g)
mutateCWithP p sd c g = callWithP p mutate c g
  where mutate (x :+ y) g = (newX :+ newY, g2)
          where (newX, g1) = normal' (x, sd) g
                (newY, g2) = normal' (y, sd) g

