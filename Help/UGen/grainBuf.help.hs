-- grainBuf ; requires=buf
let buf = control KR "buf" 0
    dur = 15
    lin a b = line KR a b dur RemoveSynth
    tr = impulse KR (lin 7.5 15) 0
    gd = lin 0.05 0.1
    r = lin 1 0.5 {- rate -}
    i = lin 0 1 {- read-location -}
    l = lin (-0.5) 0.5 {- stereo-location -}
in grainBuf 2 tr gd buf r i 2 l (-1) 512 * control KR "gain" 0.25

-- grainBuf ; requires=buf ; mouse control
let b = control KR "buf" 0
    e = -1
    x = mouseX KR (-1) 1 Linear 0.1
    y = mouseY KR 10 45 Linear 0.1
    i = impulse KR y 0
    r = linLin (lfNoise1 'α' KR 500) (-1) 1 0.5 2
    p = linLin (lfNoise2 'β' KR 0.1) (-1) 1 0 1
in grainBuf 2 i 0.1 b r p 2 x e 512 * control KR "gain" 0.25

-- grainBuf ; requires=buf ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let b = control KR "buf" 0
          e = -1
          tr = impulse AR (y * 60 + 10) 0
      in grainBuf 2 tr (ry * 0.5) b (1 + (rx * 0.1)) x 2 o e 512 * z * g
in mix (rEventVoicer 16 f) * control KR "gain" 2

---- ; load buffer
fn = "/home/rohan/data/audio/metal.wav"
fn = "/home/rohan/data/audio/instr/celeste/long/13-C4-long.wav"
fn = "/home/rohan/data/audio/instr/celeste/long/25-C5-long.wav"
fn = "/home/rohan/data/audio/instr/celeste/long/37-C6-long.wav"
fn = "/home/rohan/data/audio/instr/celeste/long/49-C7-long.wav"
withSC3 (async (b_allocRead 0 fn 0 0))
