-- mulAdd ; optimisations
The order of the multiplier inputs is significant regards multi-rate inputs?
sclang re-orders these if required.
g_04 CRASHES scsynth.

> g_04 = mulAdd 0.1 (sinOsc AR 440 0) 0.05

    {MulAdd(0.1,SinOsc.ar(440,0),0.05)}.draw

NOTE: mulAdd with I-RATE at input 0 and K-RATE thereafter CRASHES

> g_06 = sinOsc AR (mulAdd (control IR "x" 110) (control KR "y" 2) (control IR "z" 110)) 0 * 0.1


    {0.05 + (0.1 * SinOsc.ar(440,0))}.draw

