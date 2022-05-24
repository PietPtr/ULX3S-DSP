module DAC where

import Clash.Prelude
import Types

top :: HiddenClockResetEnable dom =>
    Signal dom Sample -> Signal dom DACOut
top input = (resize . flip shiftR 8) <$> input