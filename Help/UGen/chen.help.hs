-- chen
let amp = 0.5
    chen_speed_max = 0.01
    chen_sig = X.chen AR (lfCub KR 0.1 0 `in_range` (0.001,chen_speed_max)) 0.5 0.3 0.28
    minfreq = 20
    maxfreq = 2500
    sig = sinOsc AR (linExp chen_sig 0 1 minfreq maxfreq) 0
in splay sig (mceChannel 0 chen_sig) amp 0 True

-- chen
let tr = control KR "scramble" 1
    amp = 0.5
    chen_speed_max = 0.009
    chen_sig = let s = lfCub KR (lfNoise2 'α' KR (chen_speed_max * 2) * 0.1) 0
                   b = lfCub KR 0.35 0 `in_range` (0,1)
               in X.chen AR (s `in_range` (0.001,chen_speed_max)) 0.192 b 0.22
    minfreq = 40
    maxfreq = 500
    sig1 = varSaw AR (linExp chen_sig 0 1 minfreq maxfreq) 0 (lag (mceReverse chen_sig) 0.1)
    sig2 = freqShift sig1 (lag3 (X.rTScramble 'β' tr chen_sig) 0.1) 0 * 0.5
in splay (sig1 + sig2) (sum (mceChannels chen_sig) / 4) amp 0 True
