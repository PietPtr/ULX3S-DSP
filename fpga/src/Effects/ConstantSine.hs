module Effects.ConstantSine where

import Clash.Prelude
import Types

effect :: HiddenClockResetEnable dom =>
    Signal dom Sample -> Signal dom Sample
effect = mealy sine def

type SineState = (Unsigned 20, Unsigned 4)

sine :: SineState -> Sample -> (SineState, Sample)
sine (ctr, idx) _ = ((ctr', idx'), sample `shiftL` 8)
    where
        (ctr', idx') = if ctr > 5000
            then (0, idx + 1)
            else (ctr + 1, idx)

        sample = wave !! idx
        wave = 11:>14:>15:>15:>15:>14:>11:>8:>5:>3:>1:>0:>0:>2:>5:>8:>Nil

