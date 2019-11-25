module Helpers where

applyFst :: (a -> b) -> (a, c) -> (b, c)
applyFst f (a, g) = ((f a), g)

