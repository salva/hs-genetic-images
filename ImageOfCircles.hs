{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ImageOfCircles where

import System.Random

import Num
import Helpers
import Geo
import Color
import Random

type ImageOfCircles = [ Circle ]

data Circle = Circle { radius :: R
                     , center :: C
                     , color :: Color }

data Line = Line { point :: C
                 , versor :: C }


data MutationParams = MutationParams { maxRadius :: R
                                     , mutateRadiusP :: P
                                     , mutateRadiusSD :: SD
                                     , mutateCenterP :: P
                                     , mutateCenterSD :: SD
                                     , mutateColorP :: P
                                     , mutateColorSD :: SD
                                     , removeCircleP :: P
                                     , mutateCircleP :: P
                                     , addCircleP :: P }


randomLine :: (RandomGen g) => g -> (Line, g)
randomLine g = (Line {..}, g2)
  where (point, g1) = randomCInUnitSquare g
        (versor, g2) = randomVersor g1

mutateCircle :: (RandomGen g) => MutationParams -> Circle -> g -> (Circle, g)
mutateCircle (MutationParams { .. }) (Circle { .. }) g = (Circle newRadius newCenter newColor, g3)
  where (newRadius, g1) = applyFst (bounceInInterval (0.0, maxRadius)) $ mutateRWithP mutateRadiusP mutateRadiusSD radius g
        (newCenter, g2) = applyFst bounceInUnitSquare $ mutateCWithP mutateCenterP mutateCenterSD center g1
        (newColor, g3) = mutateColorWithP mutateColorP mutateColorSD color g2

randomCircle :: (RandomGen g) => MutationParams -> g -> (Circle, g)
randomCircle (MutationParams {..}) g = (Circle { .. }, g3)
  where (radius, g1) = randomR (0.0, maxRadius) g
        (center, g2) = randomCInUnitSquare g1
        (color,  g3) = randomColor g2

mutateCircles :: (RandomGen g) => MutationParams -> ImageOfCircles -> g -> (ImageOfCircles, g)
mutateCircles (mutationParams@MutationParams {..}) cs g = mutateCirclesAdd cs g
  where mutateCirclesAdd :: (RandomGen g) => ImageOfCircles -> g -> (ImageOfCircles, g)
        mutateCirclesAdd cs g =
          let (doit, g1) = randomBoolWithP addCircleP g
          in if doit
             then let (newC, g2) = randomCircle mutationParams g1
                      (newCs, g3) = mutateCirclesAdd cs g2
                  in (newC:newCs, g3)
             else mutateCirclesRemove cs g1

        mutateCirclesRemove :: (RandomGen g) => ImageOfCircles -> g -> (ImageOfCircles, g)
        mutateCirclesRemove [] g = ([], g)
        mutateCirclesRemove cs g =
          let (doit, g1) = randomBoolWithP removeCircleP g
          in if doit
             then mutateCirclesRemove (tail cs) g1
             else mutateCirclesMutate cs g1

        mutateCirclesMutate :: (RandomGen g) => ImageOfCircles -> g -> (ImageOfCircles, g)
        mutateCirclesMutate (c:cs) g = (newC:newCs, g2)
          where (newC, g1) = callWithP mutateCircleP (mutateCircle mutationParams) c g
                (newCs, g2) = mutateCirclesAdd cs g1

mergeImagesOfCircles :: (RandomGen g) => ImageOfCircles -> ImageOfCircles -> g -> (ImageOfCircles, g)
mergeImagesOfCircles left [] g = (left, g)
mergeImagesOfCircles [] right g = (right, g)
mergeImagesOfCircles left right g = (head:tail, g2)
  where (swap, g1) = randomBool g
        (head:newLeft, newRight) = if swap then (left, right) else (right, left)
        (tail, g2) = mergeImagesOfCircles newLeft newRight g1

cutImageOfCirclesByLine :: ImageOfCircles -> Line -> Bool -> ImageOfCircles
cutImageOfCirclesByLine iocs (Line {..}) dir = filter f iocs
  where f (Circle {..}) = dir == (cross < 0)
          where cross = crossProduct (center - point) versor

cutImagesOfCirclesByRandomLineAndMerge :: (RandomGen g) => ImageOfCircles -> ImageOfCircles -> g -> (ImageOfCircles, g)
cutImagesOfCirclesByRandomLineAndMerge left right g = (merged, g2)
  where (line, g1) = randomLine g
        leftCut  = cutImageOfCirclesByLine left line True
        rightCut = cutImageOfCirclesByLine right line False
        (merged, g2) = mergeImagesOfCircles leftCut rightCut g1




