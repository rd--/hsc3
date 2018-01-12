    Sound.SC3.UGen.Help.viewSC3Help "HilbertFIR"
    Sound.SC3.UGen.DB.ugenSummary "HilbertFIR"

> import Sound.SC3 {- hsc3 -}

> g_01 = hilbertFIR (sinOsc AR 100 0 * dbAmp (-20)) (localBuf 'α' 2048 1)
