module ADC where

import Clash.Prelude

type Btn = Bool
type LEDs = Unsigned 8

type State = (LEDs, Unsigned 21, Bool)


mov :: State -> (Btn, Btn) -> (State, LEDs)
mov (leds, counter, ready) input = ((leds', counter', ready'), output)
    where
        -- input sigs

        -- state
        leds' = if ready 
            then case input of
                (True, True) -> leds
                (True, False) -> rotate leds (-1)
                (False, True) -> rotate leds 1
                (False, False) -> leds
            else leds
            
        ready' = case ready of
            False -> counter == 0
            True -> not $ fst input || snd input

        counter' = counter + 1
        -- output
        output = leds

movB :: HiddenClockResetEnable dom =>
    Signal dom (Btn, Btn) -> Signal dom LEDs
movB = mealy mov (1, 0, True)