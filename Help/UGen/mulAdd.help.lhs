> import Sound.SC3 {- hsc3 -}

> g_01 = mulAdd (sinOsc AR 440 0) 0.1 0.05

These should both optimise to the same graph...

> g_02 = sinOsc AR 440 0 * 0.1 + 0.05

> g_03 = 0.05 + sinOsc AR 440 0 * 0.1

    > putStrLn $ unlines (map synthstat [g_01,g_02,g_03])

The order of the multiplier inputs is significant regards multi-rate inputs?
sclang re-orders these if required.
g_04 CRASHES scsynth.

> g_04 = mulAdd 0.1 (sinOsc AR 440 0) 0.05

    {MulAdd(0.1,SinOsc.ar(440,0),0.05)}.draw

> g_05 = 0.05 + 0.1 * sinOsc AR 440 0

    {0.05 + (0.1 * SinOsc.ar(440,0))}.draw

NOTE: mulAdd with I-RATE at input 0 and K-RATE thereafter CRASHES

> g_06 = sinOsc AR (mulAdd (control IR "x" 110) (control KR "y" 2) (control IR "z" 110)) 0 * 0.1

all AR inputs optimise ordinarily

> g_07 = (sinOsc AR 440 0 * sinOsc AR 441 0 + sinOsc AR 442 0) * 0.1

> g_08 = (sinOsc AR 440 0 + sinOsc AR 441 0 * sinOsc AR 442 0) * 0.1
