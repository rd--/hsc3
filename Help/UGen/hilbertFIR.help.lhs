> import Sound.SC3 {- hsc3 -}

composite UGen

> g_01 = hilbertFIR (sinOsc AR 100 0 * dbAmp (-20)) (localBuf 'α' 2048 1)
