-- lfPulse ; 50 Hz wave ; note SC2 had no initial phase argument
lfPulse ar 50 0 0.5 * 0.1

-- lfPulse ; modulating frequency
lfPulse ar (xLine kr 1 200 10 DoNothing) 0 0.2 * 0.1

-- lfPulse ; amplitude modulation
lfPulse kr (xLine kr 1 200 10 DoNothing) 0 0.2 * sinOsc ar 440 0 * 0.1

-- lfPulse ; used as both oscillator and lfo
lfPulse ar (lfPulse kr 3 0 0.3 * 200 + 200) 0 0.2 * 0.1

-- lfPulse ; mouse control of width
let x = mouseX kr 0 1 Linear 0.2 in lfPulse ar 220 0 x * 0.1

-- lfPulse ; compare with band limited pulse ugen
mce2 (pulse ar 100 0.3) (lfPulse ar 100 0 0.3) * 0.1

-- lfPulse ; as sum of sines ; for odd partials n amplitude is (1 / n), for even 0 ; phase is 0
let mk_freq f0 n = f0 * fromInteger n
    mk_amp n = if even n then 0 else 1 / fromInteger n
    mk_param f0 n = let m = [1,3 .. n] in zip (map (mk_freq f0) m) (map mk_amp m)
    x = midiCps (mouseX kr 20 72 Linear 0.2)
    y = mouseY kr 0.01 0.1 Exponential 0.2
    e = xLine kr 0.01 1 20 DoNothing -- xfade
    o1 = sum (map (\(fr,am) -> sinOsc ar fr 0 * am) (mk_param x 50)) * (1 - e)
    o2 = lfPulse ar x 0 0.5 * e
in mce2 o1 o2 * y

---- ; drawings ; pulse is unary, ie. range is (0,1)
Sound.Sc3.Plot.plot_ugen1 0.1 (lfPulse ar (line kr 100 800 0.1 DoNothing) 0 0.5)
