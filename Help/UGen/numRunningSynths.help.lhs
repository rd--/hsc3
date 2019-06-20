> import Sound.SC3 {- hsc3 -}

each concurrent audition increases oscillator frequency

> g_01 = sinOsc AR (numRunningSynths * 200 + 400) 0 * 0.1
