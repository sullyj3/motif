{-# LANGUAGE NumericUnderscores #-}
module Motif
    ( sleep
    , count
    , divides
    ) where

import Control.Concurrent (threadDelay)
import Data.Foldable ( Foldable(foldl') )

sleep :: Double -> IO ()
sleep seconds = threadDelay . round $ (seconds * 1_000_000)

{-# specialise count :: (a -> Bool) -> [a] -> Int #-}
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' go 0
  where go n x | p x = n+1
               | otherwise = n

{-# specialise divides :: Int -> Int -> Bool #-}
divides :: Integral a => a -> a -> Bool
divides a b = b `mod` a == 0