module Num where

import Data.Complex

type R = Double
type C = Complex R
type P = R
type SD = R

bounceInInterval :: (R, R) -> R -> R
bounceInInterval (mn, mx) v =
  if (v < mn)
  then bounceInInterval (mn, mx) (2 * mn - v)
  else if (v > mx)
       then bounceInInterval (mn, mx) (2 * mx - v)
       else v

bounceInUnitInterval = bounceInInterval (0.0, 1.0)

bounceInUnitSquare (a :+ b) = (bounceInUnitInterval a) :+ (bounceInUnitInterval b)

crossProduct (ax :+ ay) (bx :+ by) = ax * by - ay * bx
