module Recursive where

import Clash.Prelude hiding ((++), map, foldl, filter, zip, zipWith, length, splitAt)
import Data.List as L
import Data.Complex
import Debug.Trace


fft :: Int -> [(Complex Double)] -> [(Complex Double)]
fft n [] = []
fft n [wat] = []
fft n [ev, od] = [ev + expi n 0 * od, ev - expi n 0 * od]
fft n samples = upper ++ lower
    where
        upper = combineUp 0 evenffts oddffts
        lower = combineDown 0 evenffts oddffts

        evenffts = fft' $ evens samples
        oddffts = fft' $ odds samples

        (oddsUp, oddsDown) = splitHalf $ odds samples
        (evensUp, evensDown) = splitHalf $ evens samples

        combineUp k [] [] = []
        combineUp k (ev:evens) (od:odds) = (ev + expi n k * od) : combineUp (k + 1) evens odds

        combineDown k [] [] = []
        combineDown k (ev:evens) (od:odds) = (ev - expi n k * od) : combineDown (k + 1) evens odds

        splitHalf l = splitAt ((length l + 1) `div` 2) l

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
sine :: Int -> Double -> [Complex Double]
sine n freq = [sin (freq * x * mult) :+ 0 | x <- [0..2^^n - 1]]
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

        realsF = map (show . realPart) $ fft' samples
        imagsF = map (show . imagPart) $ fft' samples
        
        text = unlines $ zipWith (\a b -> a ++ "," ++ b) realsT imagsF