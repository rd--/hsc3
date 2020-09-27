import Sound.SC3 {- hsc3 -}

g_01 = let o = sinOsc AR 440 0 in o + negate o -- silence (draw graph to see Neg operator)
