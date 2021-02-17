-- warp1 ; requires=buf ; interp=2=linear
let nc = 1
    b = control KR "buf" 0
    p = linLin (lfSaw KR 0.05 0) (-1) 1 0 1
    x = mouseX KR 0.5 2 Linear 0.1
in warp1 nc b p x 0.1 (-1) 8 0.1 2

-- warp1 ; real-time (delayed) input, localBuf
let sz = 8192
    b = clearBuf (localBuf 'α' 1 sz)
    i = soundIn 0
    r = recordBuf AR b 0 1 0 1 Loop 1 DoNothing i
    ph = (sz / sampleRate) * 2 * pi
    p = lfSaw KR (1 / bufDur KR b) ph * 0.5 + 0.5
    x = mouseX KR 0.5 2 Linear 0.2
    y = mouseY KR 0.01 0.2 Linear 0.2
    w = warp1 1 b p x 0.1 (-1) 8 y 4
in mrg2 (i + w) r

-- warp1 ; requires=buf ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let b = control KR "buf" 0
          p = linLin (lfSaw KR 0.05 0) (-1) 1 0 1
          dur = linLin y 0 1 0.01 0.2
          freqScale = linLin ry 0 1 0.75 1.25
          ol = linLin rx 0 1 2 16
      in pan2 (warp1 1 b x freqScale dur (-1) ol (ry * 0.75) 2) (o * 2 - 1) (z * g)
in mix (rEventVoicer 16 f) * control KR "gain" 2

---- ; setup
fn = "/home/rohan/data/audio/instr/celeste/long/13-C4-long.wav"
fn = "/home/rohan/data/audio/instr/celeste/long/25-C5-long.wav"
withSC3 (async (b_allocRead 0 fn 0 0))
