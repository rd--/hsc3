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
    k nm def = meta_control KR nm def
in rShufflerB b
   (k "readLocationMinima" 0.0 (0,1,"lin",0.01,""))
   (k "readLocationMaxima" 0.05 (0,1,"lin",0.01,""))
   (k "readIncrementMinima" 1.99975 (0.25,4,"lin",0.01,""))
   (k "readIncrementMaxima" 2.00025 (0.25,4,"lin",0.01,""))
   (k "durationMinima" 0.25 (0,0.5,"lin",0.01,"s"))
   (k "durationMaxima" 0.30 (0,0.5,"lin",0.01,"s"))
   (k "envelopeAmplitudeMinima" 0.8 (0.5,1.0,"amp",0.01,""))
   (k "envelopeAmplitudeMaxima" 0.9 (0.5,1.0,"amp",0.01,""))
   (k "envelopeShapeMinima" 0.5 (0,1,"lin",0.01,""))
   (k "envelopeShapeMaxima" 0.6 (0,1,"lin",0.01,""))
   (k "envelopeSkewMinima" 0.4 (0,1,"lin",0.01,""))
   (k "envelopeSkewMaxima" 0.6 (0,1,"lin",0.01,""))
   (k "stereoLocationMinima" 0.0 (0,1,"lin",0.01,"")) -- ie. not (-1,1)
   (k "stereoLocationMaxima" 1.0 (0,1,"lin",0.01,""))
   (k "interOffsetTimeMinima" 0.0500 (0,0.25,"lin",0.001,"s"))
   (k "interOffsetTimeMaxima" 0.0525 (0,0.25,"lin",0.001,"s"))
   (k "ftableReadLocationIncrement" 1.0 (0.25,4.0,"lin",0.1,""))
   (k "readIncrementQuanta" 0.0 (0,1,"lin",0.1,""))
   (k "interOffsetTimeQuanta" 0.0 (0,1,"lin",0.1,""))

---- ; load buffer
ld fn = withSC3 (async (b_allocRead 0 fn 0 0))
ld "/home/rohan/data/audio/instr/crotales/crotale05(D).wav"
