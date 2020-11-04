-- localBuf ; allocate a buffer local to the synthesis graph, 1-channel, 2048
let nc = 1
    nf = 2048
    buf = localBuf 'α' nc nf
    f = fft' buf (whiteNoise 'β' AR)
    c = pv_BrickWall f (sinOsc KR 0.1 0 * 0.75)
in ifft' c * 0.1

-- localBuf ; Variant with two local buffers
let dup (p,q) f = mce2 (f p) (f q)
    n = dup ('α','β') (\z -> whiteNoise z AR)
    b = dup ('γ','δ') (\z -> localBuf z 1 2048)
    f = fft' b n
    c = pv_BrickWall f (sinOsc KR (mce2 0.1 0.11) 0 * 0.75)
in ifft' c * 0.1

-- localBuf ; not clearing the buffer accesses old data, slowly overwrite data with noise
let dup (p,q) f = mce2 (f p) (f q)
    b = localBuf 'α' 2 2048
    nf = bufFrames KR b
    x = mouseX KR 1 2 Linear 0.2
    r = playBuf 2 AR b x 1 0 Loop DoNothing * 0.1
    wr p i = bufWr b (linLin p (-1) 1 0 nf) Loop i
    n = dup ('β','γ') (\z -> whiteNoise z AR)
    ph = lfNoise0 'δ' AR 530
in mrg2 r (wr ph n)

-- localBuf ; bufCombC needs no clearing, because the delay line is filled by the ugen
let dup (p,q) f = mce2 (f p) (f q)
    d = dup ('α','β') (\z -> dust z AR 1)
    n = whiteNoise 'γ' AR * 0.1
    z = decay d 0.3 * n
    l = xLine KR 0.0001 0.01 20 DoNothing
    sr = sampleRate
    b = dup ('δ','ε') (\z -> localBuf z 2 sr)
in bufCombC b z l 0.2

-- localBuf ; asLocalBuf combines localBuf and setBuf
let b = asLocalBuf 'α' [2,1,5,3,4,0]
    x = mouseX KR 0 (bufFrames KR b) Linear 0.2
    f = indexL b x * 100 + 40
in saw AR (f * mce2 1 1.1) * 0.1

-- localBuf ; detectIndex example using local buffer
let b = asLocalBuf 'α' [2,3,4,0,1,5]
    n = bufFrames KR b
    x = floorE (mouseX KR 0 n Linear 0.1)
    i = detectIndex b x
in sinOsc AR (linExp i 0 n 200 700) 0 * 0.1

-- localBuf ; degreeToKey example ('modal space') using local buffer
let n = lfNoise1 'α' KR (mce [3,3.05])
    x = mouseX KR 0 15 Linear 0.1
    b = asLocalBuf 'β' [0,2,3.2,5,7,9,10]
    k = degreeToKey b x 12
    mk_c bf = let f0 = midiCPS (bf + k + n * 0.04)
                  o = sinOsc AR f0 0 * 0.1
                  f1 = midiCPS (mce [48,55])
                  t = lfPulse AR f1 0.15 0.5
                  f2 = midiCPS (sinOsc KR 0.1 0 * 10 + bf)
                  d = rlpf t f2 0.1 * 0.1
                  m = o + d
              in combN m 0.31 0.31 2 + m
in (mk_c 48 + mk_c 72) * 0.25
