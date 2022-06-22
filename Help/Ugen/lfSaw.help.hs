-- lfSaw ; note SC2 did not have the initial phase argument
lfSaw ar 500 1 * 0.025

-- lfSaw ; used as both Oscillator and LFO
lfSaw ar (lfSaw kr 4 0 * 400 + 400) 0 * 0.025

-- lfSaw ; output range is bi-polar
let f = mce [linLin (lfSaw kr 0.5 0) (-1) 1 200 1600, 200, 1600]
    a = mce [0.1,0.05,0.05]
in mix (sinOsc ar f 0 * a)

-- lfSaw ; saw-tooth wave as sum of sines, for all partials n amplitude is (1 / n) ; phase is always 0
let mk_freq f0 n = f0 * fromInteger n
    mk_amp n = 1 / fromInteger n
    mk_param f0 n = let m = [1,2 .. n] in zip (map (mk_freq f0) m) (map mk_amp m)
    x = midiCps (mouseX kr 20 72 Linear 0.2)
    y = mouseY kr 0.01 0.1 Exponential 0.2
    e = xLine kr 0.01 1 20 DoNothing -- cross-fade from sum to lfSaw
    o1 = sum (map (\(fr,am) -> sinOsc ar fr 0 * am) (mk_param x 25)) * (1 - e)
    o2 = lfSaw ar x 0 * e
in mce2 o1 o2 * y

-- lfSaw ; as phasor input to sin function
sin (range 0 two_pi (lfSaw ar 440 0)) * 0.1

-- lfSaw ; mixed with sin, then with distortions
let f = xLine kr 220 440 10 DoNothing
    o1 = sinOsc ar (f + mce2 0 0.7) 0
    o2 = lfSaw ar (f + mce2 0 0.7) 0 * 0.3
    o3 = o1 + o2
    o4 = cubed (distort (log (distort o3)))
in o4 * 0.05

-- lfSaw
let f = sinOsc kr (mce [0.16,0.33,0.41]) 0 * 10 + mce [1,1.1,1.5,1.78,2.45,6.7,8] * 220
in mix (lfSaw ar f 0) * 0.1

-- lfSaw ; ln 2021-04-08 https://lukasnowok.github.io/spectrology/ ; ~= [0.1,0.15,0.225,0.3375]
let o = lfSaw ar (lfSaw ar (mce (take 4 (iterate (* 1.5) 0.1))) 0 * 5000) 0 * 500
in mix (sinOsc ar (1000 + o) 0) * 1/4 * 0.1

---- ; drawings
UI.ui_baudline 4096 50 "linear" 2
Sound.Sc3.Plot.plot_ugen1 0.1 (lfSaw ar 50 0) -- ascending
Sound.Sc3.Plot.plot_ugen1 0.002 (lfSaw ar 5000 0)
