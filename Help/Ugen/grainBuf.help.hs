-- grainBuf ; requires=buf
let (buf, nc) = (control kr "buf" 100, 2)
    dur = 15
    lin a b = line kr a b dur RemoveSynth
    tr = impulse kr (lin 7.5 15) 0
    gd = lin 0.05 0.1
    r = lin 1 0.5 {- rate -}
    i = lin 0 1 {- read-location -}
    l = lin (-0.5) 0.5 {- stereo-location -}
in grainBuf nc tr gd buf r i 2 l (-1) 512 * control kr "gain" 0.25

-- grainBuf ; requires=buf ; mouse control
let (buf, nc) = (control kr "buf" 100, 2)
    e = -1
    x = mouseX kr (-1) 1 Linear 0.1
    y = mouseY kr 10 45 Linear 0.1
    i = impulse kr y 0
    r = linLin (lfNoise1Id 'α' kr 500) (-1) 1 0.5 2
    p = linLin (lfNoise2Id 'β' kr 0.1) (-1) 1 0 1
in grainBuf 2 i 0.1 buf r p 2 x e 512 * control kr "gain" 0.25

-- grainBuf ; requires=buf ; event control
let f (_,g,x,y,z,o,rx,ry,_,_,_) =
      let (buf, nc) = (control kr "buf" 100, 2)
          e = -1
          tr = impulse ar (y * 60 + 10) 0
      in grainBuf nc tr (ry * 0.5) buf (1 + (rx * 0.1)) x 2 o e 512 * z * g
in mix (voicer 16 f) * control kr "gain" 2

---- ; load buffer
ld fn = withSc3 (async (b_allocRead 0 (sfResolve fn) 0 0))
ld "pf-c5.aif" -- mono
ld "metal.wav" -- mono
ld "instr/celeste/long/13-C4-long.wav" -- mono
ld "instr/celeste/long/25-C5-long.wav"
ld "instr/celeste/long/37-C6-long.wav"
ld "instr/celeste/long/49-C7-long.wav"

fn = "/home/rohan/uc/the-center-is-between-us/visitants/flac/f/y.flac"
withSc3 (async (b_allocRead 0 fn (48000 * 45) (48000 * 1)))
