module Main where

import TowardsHW

import Clash.Prelude hiding (
    (++), map, foldl, filter, zip, zipWith, length, splitAt, zipWith3, take, 
    repeat, drop)
import Data.List as L
import Data.Complex
import Debug.Trace
import Prelude (getLine)

main = do
    samples <- convert <$> getMultipleLines 1024
    mapM_ print (applyFft samples)
    where
        convert :: [String] -> [Complex Double]
        convert xs = map c xs
            where c str = (read str :: Double) :+ 0

        -- returns magnitudes after fft
        applyFft :: [Complex Double] -> [Double]
        applyFft samples = map magnitude $ fft' samples


getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        return (x:xs)