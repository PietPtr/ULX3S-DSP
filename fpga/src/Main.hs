module Main where

import Clash.Prelude

import qualified Data.List as L
import Types

import qualified ADC
import qualified Effects.Amplify
import qualified Effects.ConstantSine
import qualified DAC

system :: HiddenClockResetEnable dom =>
    Signal dom (Bit) -> Signal dom (Bool, Bit, Bit, DACOut, Bit)
system miso = bundle (csn, mosi, sclk, dac, miso)
    where
        dac = DAC.top sample
        -- sample = Effects.ConstantSine.effect (pure 1)
        sample = adcSample -- the identity effect 
        (csn, mosi, sclk, adcSample) = unbundle $ ADC.top' miso

{-# ANN topEntity
  (Synthesize
    { t_name   = "main"
    , t_inputs = [ PortName "clk_25mhz"
                 , PortName "reset"
                 , PortName "enable"
                 , PortName "adc_miso"]
    , t_output = PortProduct "" 
        [ PortName "adc_csn"
        , PortName "adc_mosi"
        , PortName "adc_sclk"
        , PortName "led"
        , PortName "adc_miso_monitor"]
    }) #-}
topEntity ::
     Clock System
     -> Reset System
     -> Enable System
     -> Signal System (Bit)
     -> Signal System (Bool, Bit, Bit, DACOut, Bit)
topEntity = exposeClockResetEnable system

main = print "Compile this hardware description to Verilog with Clash."