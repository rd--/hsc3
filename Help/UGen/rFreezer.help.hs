-- rFreezer ; check buffer
let b = control kr "buf" 0
    s = bufRateScale kr b
in playBuf 1 ar b s 1 0 NoLoop RemoveSynth * 0.1

-- rFreezer ; static instance
let b = control kr "buf" 0
in X.rFreezer b 0.35 0.4 0.6 0.1 0.05 0.02 0.1 0 0 6

-- rFreezer ; static instance
let b = control kr "buf" 0
in X.rFreezer b 0.3 0.4 0.6 1 0 0 0 0 0 6

-- rFreezer ; static instance
let b = control kr "buf" 0
in X.rFreezer b 0.3 0.7 0.6 0.35 0 0.5 0.5 0 0 6

-- rFreezer ; static instance
let b = control kr "buf" 0
in X.rFreezer b 0.2500 0.2505 0.1 1 0 0.050 0.005 0 0 24

-- rFreezer ; k-rate instance
let b = control kr "buf" 0
    n f i j = linLin (lfNoise2 kr f) (-1) 1 i j
    left = n 1 0.3 0.8
    right = left + n 1 0.01 0.05
in X.rFreezer b left right 0.1 0.5 0.1 0.5 0.05 0 0 24

-- rFreezer ; k-rate instance ; id
let b = control kr "buf" 0
    nId z f i j = linLin (lfNoise2Id z kr f) (-1) 1 i j
    left = nId 'α' 1 0.3 0.8
    right = left + nId 'β' 1 0.01 0.05
in X.rFreezer b left right 0.1 0.5 0.1 0.5 0.05 0 0 24

-- rFreezer ; k-rate instance
let b = control kr "buf" 0
    n i j = linLin (lfNoise2 kr 0.1) (-1) 1 i j
in X.rFreezer b (n 0.3 0.4) (n 0.5 0.6) (n 0.3 0.6) (n 0.95 1.05) (n 0.05 0.15) (n 0.05 0.15) (n 0.05 0.15) 0 0 36

-- rFreezer ; k-rate instance ; id
let b = control kr "buf" 0
    nId z i j = linLin (lfNoise2Id z kr 0.1) (-1) 1 i j
in X.rFreezer b (nId 'α' 0.3 0.4) (nId 'β' 0.5 0.6) (nId 'γ' 0.3 0.6) (nId 'δ' 0.95 1.05) (nId 'ε' 0.05 0.15) (nId 'ζ' 0.05 0.15) (nId 'η' 0.05 0.15) 0 0 36

-- rFreezer ; controls
let b = control_m kr "buf" 0 (0,0,"lin")
    (lhs,rhs) = control_rng kr "wnd" (0,1) (0,1,"lin")
    amp = control_m kr "amp" 0.1 (0,1,"amp")
    incr = control_m kr "incr" 1 (0,4,"lin")
    incrO = control_m kr "incrO" 0 (0,1,"lin")
    incrR = control_m kr "incrR" 0 (0,1,"lin")
    wndR = control_m kr "wndR" 0 (0,1,"lin")
    syncPh = control_m kr "syncPh" 0 (0,1,"trigger")
    randPh = control_m kr "randPh" 0 (0,1,"trigger")
    dgr = control_m kr "numLp" 24 (1,64,"lin")
in X.rFreezer b lhs rhs amp incr incrO incrR wndR syncPh randPh dgr

---- ; allocate buffer 0, required for examples
ld fn = withSC3 (async (b_allocRead 0 (sfResolve fn) 0 0))
ld "crotale05(D).wav"
ld "saron-panerus-S-0-5.flac"
ld "saron-panerus-S-0-6.flac"
ld "saron-barung-S-0-1.flac"
ld "saron-demung-S-1-3.flac"
ld "saron-demung-S-1-5.flac"
ld "saron-demung-S-1-6.flac"
ld "bonang-barung-S-0-2.flac"
ld "bonang-barung-S-2-1.flac"
ld "gender-barung-S-1-2.flac"
ld "gender-barung-S-2-5.flac"
ld "gender-barung-S-3-3.flac"

{---- ; RFreeze ; concurrent loops at a signal buffer

Create a set of concurrent loops at a signal buffer.  This is the
dynamic and gestural variant of RLoopSet.  It was written after
reading the manual for the GRM ToolsId 'Freeze' plugin.
-}
