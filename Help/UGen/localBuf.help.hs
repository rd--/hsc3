-- localBuf ; allocate a buffer local to the synthesis graph, 1-channel, 2048
let nc = 1
    nf = 2048
    buf = localBufId 'α' nc nf
    f = fft' buf (whiteNoiseId 'β' ar)
    c = pv_BrickWall f (sinOsc kr 0.1 0 * 0.75)
in ifft' c * 0.1

-- localBuf ; Variant with two local buffers
let dup (p,q) f = mce2 (f p) (f q)
    n = dup ('α','β') (\z -> whiteNoiseId z ar)
    b = dup ('γ','δ') (\z -> localBufId z 1 2048)
    f = fft' b n
    c = pv_BrickWall f (sinOsc kr (mce2 0.1 0.11) 0 * 0.75)
in ifft' c * 0.1

-- localBuf ; not clearing the buffer accesses old data, slowly overwrite data with noise
let dup (p,q) f = mce2 (f p) (f q)
    b = localBufId 'α' 2 2048
    nf = bufFrames kr b
    x = mouseX kr 1 2 Linear 0.2
    r = playBuf 2 ar b x 1 0 Loop DoNothing * 0.1
    wr p i = bufWr b (linLin p (-1) 1 0 nf) Loop i
    n = dup ('β','γ') (\z -> whiteNoiseId z ar)
    ph = lfNoise0Id 'δ' ar 530
in mrg2 r (wr ph n)

-- localBuf ; bufCombC needs no clearing, because the delay line is filled by the ugen
let dup (p,q) f = mce2 (f p) (f q)
    d = dup ('α','β') (\z -> dustId z ar 1)
    n = whiteNoiseId 'γ' ar * 0.1
    z = decay d 0.3 * n
    l = xLine kr 0.0001 0.01 20 DoNothing
    sr = sampleRate
    b = dup ('δ','ε') (\z -> localBufId z 2 sr)
in bufCombC b z l 0.2

-- localBuf ; asLocalBuf combines localBuf and setBuf
let b = asLocalBuf [2,1,5,3,4,0]
    x = mouseX kr 0 (bufFrames kr b) Linear 0.2
    f = indexL b x * 100 + 40
in saw ar (f * mce2 1 1.1) * 0.1

-- localBuf ; asLocalBuf combines localBuf and setBuf ; id
let b = asLocalBufId 'α' [2,1,5,3,4,0]
    x = mouseX kr 0 (bufFrames kr b) Linear 0.2
    f = indexL b x * 100 + 40
in saw ar (f * mce2 1 1.1) * 0.1

-- localBuf ; detectIndex example using local buffer
let b = asLocalBufId 'α' [2,3,4,0,1,5]
    n = bufFrames kr b
    x = floorE (mouseX kr 0 n Linear 0.1)
    i = detectIndex b x
in sinOsc ar (linExp i 0 n 200 700) 0 * 0.1

-- localBuf ; degreeToKey example ('modal space') using local buffer
let n = lfNoise1Id 'α' kr (mce [3,3.05])
    x = mouseX kr 0 15 Linear 0.1
    b = asLocalBufId 'β' [0,2,3.2,5,7,9,10]
    k = degreeToKey b x 12
    mk_c bf = let f0 = midiCps (bf + k + n * 0.04)
                  o = sinOsc ar f0 0 * 0.1
                  f1 = midiCps (mce [48,55])
                  t = lfPulse ar f1 0.15 0.5
                  f2 = midiCps (sinOsc kr 0.1 0 * 10 + bf)
                  d = rlpf t f2 0.1 * 0.1
                  m = o + d
              in combN m 0.31 0.31 2 + m
in (mk_c 48 + mk_c 72) * 0.25
