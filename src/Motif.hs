{-# LANGUAGE NumericUnderscores #-}
module Motif where

import Control.Concurrent (threadDelay)
import Data.Foldable ( Foldable(foldl') )

sleep :: Double -> IO ()
sleep seconds = threadDelay . round $ (seconds * 1_000_000)

{-# specialise count :: (a -> Bool) -> [a] -> Int #-}
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' go 0
  where go n x | p x = n+1
               | otherwise = n

{-# specialize replace :: Eq a => a -> a -> [a] -> [a] #-}
replace :: (Eq a, Functor f) => a -> a -> f a -> f a
replace a b = fmap (\x -> if x == a then b else x)

{-# specialise divides :: Int -> Int -> Bool #-}
divides :: Integral a => a -> a -> Bool
divides a b = b `mod` a == 0
