-- metal plate (jmcc) #4
let
    enumFromN e i = let j = fromEnum e in [j .. j + i]
    sr = 48000::Double
    n = 4 :: Int {- number of delay lines -}
    maxdt = ceiling (sr * 0.03) :: Int {- maximum delay time -}
    mk_buf k = asLocalBuf k (replicate maxdt 0)
    buf = map mk_buf [0 .. n - 1] {- buffers for delay lines -}
    tap_tm = map (\z -> rand ('α',z) 0.015 0.03) (enumFromN 'β' n) {- random tap times -}
    exc_freq = mouseY KR 10 8000 Linear 0.2
    exc_trig = impulse AR 0.5 0 * 0.2
    exc = decay2 exc_trig 0.01 0.2 * lfNoise2 'γ' AR exc_freq {- excitation -}
    del = zipWith (tap 1 AR) buf tap_tm {- delay line taps -}
    flt_freq = mouseX KR 10 5000 Linear 0.2
    flt = map (\i -> lpf i flt_freq * 0.98) del {- tap filters -}
    wr_f b f = recordBuf AR b 0 1 0 1 Loop 1 DoNothing (f + exc)
    wr = zipWith wr_f buf flt {- write to delay lines -}
in mrg (sum flt : wr)
