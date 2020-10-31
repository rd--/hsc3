-- balance2
let l = lfSaw AR 44 0
    r = pulse AR 33 0.5
    p = fSinOsc KR 0.5 0
in balance2 l r p 0.1

-- balance2
let [s0,s1] = mceChannels (sinOsc AR (mce2 440 550) 0)
    n = lfNoise0 'α' KR 4
in balance2 s0 s1 n 0.3

-- balance2
let s = sinOsc AR 440 0
    p = sinOsc KR 0.2 0
in balance2 s s p 1 * 0.2

-- balance2
let s = sinOsc AR 440 0
    p = sinOsc KR 0.2 0
in balance2 s s p 0.2

-- balance2
let s0 = sinOsc AR 440 0
    s1 = sinOsc AR 550 0
    x = mouseX KR (-1) 1 Linear 0.2
in balance2 s0 s1 x 0.2

-- balance2
let s = soundIn 0
    l = lpf s 500
    h = s - l
    n = lfNoise0 'α' KR 4
in balance2 l h n 0.3
