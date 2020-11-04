-- warp1 ; requires=buf
let b = control KR "buf" 0
    p = linLin (lfSaw KR 0.05 0) (-1) 1 0 1
    x = mouseX KR 0.5 2 Linear 0.1
in warp1 1 b p x 0.1 (-1) 8 0.1 2

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
