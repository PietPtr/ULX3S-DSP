{-# LANGUAGE NumericUnderscores #-}

module ADC where

import Clash.Prelude
import Types
import Debug.Trace

-- top :: HiddenClockResetEnable dom =>
--     Signal dom Bit -> Signal dom (Bool, Bit, Bit, Sample)
-- top miso = bundle (csn, mosi, sclk, sample)
--     where
--         (sclk, csn, maybeSample, mosi) = unbundle $ mealy logger (Start maxBound) miso
--         sample = regMaybe 0 maybeSample



top' :: HiddenClockResetEnable dom =>
    Signal dom Bit -> Signal dom (Bool, Bit, Bit, Sample)
top' miso = out
    where
        ctr = register (0 :: Unsigned 8) ctr'
        ctr' = (+1) <$> ctr

        enable = (== 0) <$> ctr
        out = (exposeEnable top) (toEnable enable) miso

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
-- ECHO: 1000_0000_0000_0100

top :: HiddenClockResetEnable dom =>
    Signal dom Bit -> Signal dom (Bool, Bit, Bit, Sample)
top miso = bundle (csn, mosi, sclk, resize <$> sample)
    where
        setting = register (0 :: Index 4) setting'
        setting' = f <$> setting <*> ready

        f setting ready = if ready && setting < maxBound
            then setting + 1
            else setting

        config =
            0b1000_0000_0000_0100 :>
            0b0000_0000_0000_0000 :>
            0b0111_1111_1111_1111 :> Nil
            -- (0b1000_0000_0000_0000 :: Unsigned 16) :>
            -- 0b0000_1000_0000_0000 :>
            -- 2 `shiftL` 5 :>
            -- 0x12 `shiftL` 11 :>
            -- (0x11 `shiftL` 11 .|. 0b100) :>
            -- 0b0010_0000_0000_0010 :> Nil

        (ready, sclk, csn, mosi, maybeSample) = unbundle $ 
            mealy spiSender Idle (bundle $ (miso, settingWord))

        settingWord = g <$> ready <*> setting

        sample = regMaybe 0 maybeSample

        g ready setting = case (ready, setting == maxBound) of
            (False, _) -> Nothing
            (True, False) -> Just $ config !! setting
            (True, True) -> Just 0b101010101010
        


sendCounter :: HiddenClockResetEnable dom =>
    Signal dom Bit -> Signal dom (Bool, Bit, Bit, Sample)
sendCounter miso = bundle (csn, mosi, sclk, (resize . maybe 0 id) <$> maybeSample)
    where
        toSend = register (0 :: Unsigned 16) toSend'
        toSend' = (+1) <$> toSend
        maybeFied = toMaybe <$> ready <*> toSend

        (ready, sclk, csn, mosi, maybeSample) = unbundle $ mealy spiSender Idle (bundle $ (miso, maybeFied))

        toMaybe b a = if b then Just a else Nothing


data SPISendState
    --        sclk         bit ctr    data out      data in
    = Sending (Unsigned 2) (Index 16) (Unsigned 16) (Unsigned 16)
    | Idling (Unsigned 4)
    | Idle
    deriving (Eq, NFDataX, Generic, Show)

spiSender :: SPISendState -> (Bit, Maybe (Unsigned 16)) -> (SPISendState, (Bool, Bit, Bool, Bit, Maybe (Unsigned 16)))
spiSender state (miso, word) = (state', (ready, sclk, csn, mosi, sample))
    where
        ready = state == Idle
        mosi = case state of
            Sending clk ctr word _ -> fromIntegral (word `shiftR` 15)
            _ -> 0

        sclk = case state of
            Sending clk _ _ _ -> case clk of
                0b00 -> 0
                0b01 -> 1
                0b10 -> 1
                0b11 -> 0
            _ -> 0

        csn = case state of
            Sending 3 15 _ _ -> True
            Sending 2 15 _ _ -> True
            Idling _ -> True
            Idle -> True
            _ -> False

        sample = case state of
            Sending _ 0 _ wordIn -> Just wordIn
            _ -> Nothing
        
        state' = case state of
            Sending _ 0 _ _ -> Idling maxBound
            Sending 0 ctr wordOut wordIn -> 
                Sending maxBound (ctr - 1) (wordOut `shiftL` 1) (wordIn `shiftL` 1 .|. fromIntegral miso)
            Sending clk ctr wordOut wordIn -> Sending (clk - 1) ctr wordOut wordIn

            Idling 0 -> Idle
            Idling n -> Idling (n - 1)

            Idle -> case word of
                Just w -> Sending maxBound maxBound w 0
                Nothing -> Idle

