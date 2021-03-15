-- rFreezer ; check buffer
let b = control KR "buf" 0
    s = bufRateScale KR b
in playBuf 1 AR b s 1 0 NoLoop RemoveSynth * 0.1

-- rFreezer ; static instance
let b = control KR "buf" 0
in X.rFreezer b 0.35 0.4 0.6 0.1 0.05 0.02 0.1 0 0 6

-- rFreezer ; static instance
let b = control KR "buf" 0
in X.rFreezer b 0.3 0.4 0.6 1 0 0 0 0 0 6

-- rFreezer ; static instance
let b = control KR "buf" 0
in X.rFreezer b 0.3 0.7 0.6 0.35 0 0.5 0.5 0 0 6

-- rFreezer ; static instance
let b = control KR "buf" 0
in X.rFreezer b 0.2500 0.2505 0.1 1 0 0.050 0.005 0 0 24

-- rFreezer ; k-rate instance
let b = control KR "buf" 0
    n z f i j = linLin (lfNoise2 z KR f) (-1) 1 i j
    left = n 'α' 1 0.3 0.8
    right = left + n 'β' 1 0.01 0.05
in X.rFreezer b left right 0.1 0.5 0.1 0.5 0.05 0 0 24

-- rFreezer ; k-rate instance
let b = control KR "buf" 0
    n z i j = linLin (lfNoise2 z KR 0.1) (-1) 1 i j
in X.rFreezer b (n 'α' 0.3 0.4) (n 'β' 0.5 0.6) (n 'γ' 0.3 0.6) (n 'δ' 0.95 1.05) (n 'ε' 0.05 0.15) (n 'ζ' 0.05 0.15) (n 'η' 0.05 0.15) 0 0 36

-- rFreezer ; controls
let b = control_md KR "buf" 0 (0,0,"lin",1,"")
    (lhs,rhs) = control_rng KR "wnd" (0,1) (0,1,"lin",0,"")
    amp = control_md KR "amp" 0.1 (0,1,"amp",0,"")
    incr = control_md KR "incr" 1 (0,4,"lin",0,"")
    incrO = control_md KR "incrO" 0 (0,1,"lin",0,"")
    incrR = control_md KR "incrR" 0 (0,1,"lin",0,"")
    wndR = control_md KR "wndR" 0 (0,1,"lin",0,"")
    syncPh = control_md KR "syncPh" 0 (0,1,"tr",1,"")
    randPh = control_md KR "randPh" 0 (0,1,"tr",1,"")
    dgr = control_md KR "numLp" 24 (1,64,"lin",1,"")
in X.rFreezer b lhs rhs amp incr incrO incrR wndR syncPh randPh dgr

---- ; allocate buffer 0, required for examples
ld fn = withSC3 (async (b_allocRead 0 fn 0 0))
ld "/home/rohan/data/audio/instr/crotales/crotale05(D).wav"
ld "/home/rohan/uc/sp-id/flac/gamelan/suhirdjan/saron-panerus-S-0-5.flac"
ld "/home/rohan/uc/sp-id/flac/gamelan/suhirdjan/saron-panerus-S-0-6.flac"
ld "/home/rohan/uc/sp-id/flac/gamelan/suhirdjan/saron-barung-S-0-1.flac"
ld "/home/rohan/uc/sp-id/flac/gamelan/suhirdjan/saron-demung-S-1-3.flac"
ld "/home/rohan/uc/sp-id/flac/gamelan/suhirdjan/saron-demung-S-1-5.flac"
ld "/home/rohan/uc/sp-id/flac/gamelan/suhirdjan/saron-demung-S-1-6.flac"
ld "/home/rohan/uc/sp-id/flac/gamelan/suhirdjan/bonang-barung-S-0-2.flac"
ld "/home/rohan/uc/sp-id/flac/gamelan/suhirdjan/bonang-barung-S-2-1.flac"
ld "/home/rohan/uc/sp-id/flac/gamelan/suhirdjan/gender-barung-S-1-2.flac"
ld "/home/rohan/uc/sp-id/flac/gamelan/suhirdjan/gender-barung-S-2-5.flac"
ld "/home/rohan/uc/sp-id/flac/gamelan/suhirdjan/gender-barung-S-3-3.flac"

{---- ; RFreeze ; concurrent loops at a signal buffer

Create a set of concurrent loops at a signal buffer.  This is the
dynamic and gestural variant of RLoopSet.  It was written after
reading the manual for the GRM Tools 'Freeze' plugin.
-}
