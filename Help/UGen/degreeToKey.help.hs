-- degreeToKey ; modal space, mouse x controls discrete pitch in dorian mode
let buf = (asLocalBuf 'β' [0,2,3.2,5,7,9,10])
    x = mouseX KR 0 15 Linear 0.1
    k = degreeToKey buf x 12
    f b = let n = lfNoise1 'α' KR (mce [3,3.05])
              o = sinOsc AR (midiCPS (b + k + n * 0.04)) 0 * 0.1
              t = lfPulse AR (midiCPS (mce [48,55])) 0.15 0.5
              d = rlpf t (midiCPS (sinOsc KR 0.1 0 * 10 + b)) 0.1 * 0.1
              m = o + d
          in combN m 0.31 0.31 2 + m
in (f 48 + f 72) * 0.25
