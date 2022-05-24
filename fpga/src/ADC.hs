module ADC where

import Clash.Prelude
import Types

top :: HiddenClockResetEnable dom =>
    Signal dom Bit -> Signal dom (Bool, Bit, Bit, Sample)
top miso = bundle (csn, mosi, sclk, sample)
    where
        mosi = pure 0

        (sclk, csn, maybeSample) = unbundle $ mealy logger (Start) miso
        sample = regMaybe 0 maybeSample

{-
ADC Configuration register:
    REFSEL = 0 (default)
Unipolar register:
    zet alle bits naar 0 zodat elk channel single ended doet (is al default)
Bipolar register:
    zet alle bits naar 0 voor unipolar (default)

ADC Mode Control
    SCAN[3:0] = 0001 (default)
    CHSEL[3:0] = 0000 (channel 0 dus, default)
    PM[1:0] = 00 (altijd power on, default)
-}


data LogState
    = Start
    | Reading (Unsigned 2) (Index 17) (Unsigned 16)
    | Forwarding (Unsigned 16)
    deriving (Show, Generic, NFDataX)

logger :: LogState -> Bit -> (LogState, (Bit, Bool, Maybe Sample))
logger state miso = (state', (clk_out, csn, sample))
    where
        state' = case state of
            Start -> Reading maxBound maxBound def
            Reading 0 ctr d -> if ctr == 0
                then Forwarding d
                else Reading maxBound (ctr - 1) vec
                    where
                        vec = d `shiftL` 1 .|. (fromIntegral miso)
            Reading n ctr d -> Reading (n-1) ctr d
            Forwarding d -> Reading maxBound maxBound maxBound

        csn = case state of
            Start -> True
            _ -> False
        
        clk_out = case state of
            Reading clk _ _ -> fromIntegral $ clk `shiftR` 1
            _ -> 0

        sample = case state of
            Reading _ _ _ -> Nothing
            Forwarding d -> Just $ resize $ d `shiftR` 3