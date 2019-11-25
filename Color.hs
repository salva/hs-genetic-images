{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Color where

import System.Random
import Data.Random.Normal

import Num
import Helpers
import Random

data Color = Color { red :: R
                   , green :: R
                   , blue :: R }

mutateColorWithP :: (RandomGen g) => P -> SD -> Color -> g -> (Color, g)
mutateColorWithP p sd color g = callWithP p mutate color g
  where mutate (Color {..}) g = (Color newRed newGreen newBlue, g3)
          where bounce = applyFst bounceInUnitInterval
                (newRed,   g1) = bounce $ normal' (red,   sd) g
                (newGreen, g2) = bounce $ normal' (green, sd) g1
                (newBlue,  g3) = bounce $ normal' (blue,  sd) g2

randomColor g = (Color {..}, g3)
  where (red,   g1) = randomR (0.0, 1.0) g
        (green, g2) = randomR (0.0, 1.0) g1
        (blue,  g3) = randomR (0.0, 1.0) g2

