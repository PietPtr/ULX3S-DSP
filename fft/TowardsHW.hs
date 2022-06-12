module Recursive where

import Clash.Prelude hiding ((++), map, foldl, filter, zip, zipWith, length, splitAt, zipWith3)
import Data.List as L
import Data.Complex
import Debug.Trace


fft :: Int -> [(Complex Double)] -> [(Complex Double)]
fft n [] = []
fft n [wat] = []
fft n [ev, od] = [ev + expi n 0 * od, ev - expi n 0 * od]
fft n samples = upper ++ lower
    where
        upper = combineUp 1 evenffts oddffts
        lower = combineUp (-1) evenffts oddffts

        evenffts = fft' $ evens samples
        oddffts = fft' $ odds samples

        combineUp signum = zipWith3 combine [0..]
            where combine k ev od = ev + signum * expi n k * od


fft' :: [Complex Double] -> [Complex Double]
fft' samples = fft (L.length samples) samples

-- returns all elements with odd numbered index, assumes 0 indexed list
odds :: [a] -> [a]
odds [] = []
odds [a] = []
odds (ev:od:rest) = od : odds rest

-- returns all elements with even numbered index, assumes 0 indexed list
evens :: [a] -> [a]
evens [] = []
evens [a] = [a]
evens (ev:_:rest) = ev : evens rest

expi :: Int -> Int -> Complex Double
-- expi n 0 = 1 :+ 0 -- TODO: proof
expi n k = exp $ 0 :+ (twopii * fromIntegral k)
    where
        twopii = (- 2 * pi / fromIntegral n)

-- signal generators, n is taken to the power of 2 for the amount of samples
sine :: Int -> Double -> Double -> [Complex Double]
sine n amp freq = [amp * sin (freq * x * mult) :+ 0 | x <- [0..2^^n - 1]]
    where
        mult = 2*pi

(+++) :: [Complex Double] -> [Complex Double] -> [Complex Double]
as +++ bs = zipWith (+) as bs

-- write the real part and imaginary part to a csv for inspection
csv :: [Complex Double] -> IO ()
csv samples = writeFile "samples.csv" text
    where
        reals = map (show . realPart) samples
        imags = map (show . imagPart) samples
        
        text = unlines $ zipWith (\a b -> a ++ "," ++ b) reals imags

csvfft :: [Complex Double] -> IO ()
csvfft samples = writeFile "fft.csv" text
    where
        realsT = map (show . realPart) samples
        imagsT = map (show . imagPart) samples

        freq = map (show . magnitude) $ fft' samples
        
        text = unlines $ zipWith (\a b -> a ++ "," ++ b) realsT freq