module Main where

import Clash.Prelude

import qualified Data.List as L
import qualified ADC




{-# ANN topEntity
  (Synthesize
    { t_name   = "main"
    , t_inputs = [ PortName "clk_25mhz"
                 , PortName "reset"
                 , PortName "enable"
                 , PortName "btn"]
    , t_output = PortName "led"
    }) #-}
topEntity ::
     Clock System
     -> Reset System
     -> Enable System
     -> Signal System (ADC.Btn, ADC.Btn)
     -> Signal System ADC.LEDs
topEntity = exposeClockResetEnable ADC.movB

main = print "Compile this hardware description to Verilog with Clash."