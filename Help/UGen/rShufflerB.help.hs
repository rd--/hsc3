-- playBuf ; check buffer
let b = control KR "buf" 0
    s = bufRateScale KR b
in playBuf 1 AR b s 1 0 NoLoop RemoveSynth * 0.1

-- rShufflerB ; static
let b = control KR "buf" 0
in X.rShufflerB b 0 0.05 0.95 1.05 0.035 0.05 0.1 0.2 0.4 0.6 0.4 0.6 0 1 0.005 0.01 1 0 0

-- rShufflerB ; static, quantized
let b = control KR "buf" 0
in X.rShufflerB b 0 1 0.5 2 0.025 0.075 0.2 0.6 0.1 0.9 0.1 0.9 0 1 0.005 0.05 0 0.5 0.005

-- rShufflerB ; static, pointilist
let b = control KR "buf" 0
in X.rShufflerB b 0.4 0.5 0.5 2 0.05 0.15 0.2 0.5 0.3 0.7 0.3 0.7 0 1 0.05 0.25 0 0 0

-- rShufflerB ; controls
let b = control KR "buf" 0
    k nm def = control_md KR nm def
    k2 nm (d1,d2) = control_rng KR nm (d1,d2)
    k2_mul m (c1,c2) = (c1 * m,c2 * m)
    rShufflerB_grp p1 (p2,p3) (p4,p5) (p6,p7) (p8,p9) (p10,p11) (p12,p13) (p14,p15) (p16,p17) p18 p19 p20 = X.rShufflerB p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20
in rShufflerB_grp b
   (k2 "readLocation" (0.0,0.5) (0,1,"lin",0.01,""))
   (k2 "readIncrement" (1.99975,2.00025) (0.25,4,"lin",0.01,""))
   (k2_mul 0.001 (k2 "duration" (250,300) (1,500,"lin",0.1,"ms")))
   (k2 "envelopeAmplitude" (0.8,0.9) (0.5,1.0,"amp",0.01,""))
   (k2 "envelopeShape" (0.5,0.6) (0,1,"lin",0.01,""))
   (k2 "envelopeSkew" (0.4,0.6) (0,1,"lin",0.01,""))
   (k2 "stereoLocation" (0.0,1.0) (0,1,"lin",0.01,"")) -- ie. not (-1,1)
   (k2_mul 0.001 (k2 "interOffsetTime" (50,52.5) (1,250,"lin",0.1,"ms")))
   (k "ftableReadLocationIncrement" 1.0 (0.25,4.0,"lin",0.1,""))
   (k "readIncrementQuanta" 0.0 (0,1,"lin",0.1,""))
   (k "interOffsetTimeQuanta" 0.0 (0,1,"lin",0.1,""))

---- ; load buffer
ld fn = withSC3 (async (b_allocRead 0 fn 0 0))
ld "/home/rohan/data/audio/instr/crotales/crotale05(D).wav"
