-- balance2
let l = lfSaw ar 44 0
    r = pulse ar 33 0.5
    p = fSinOsc kr 0.5 0
in balance2 l r p 0.1

-- balance2
let [s0,s1] = mceChannels (sinOsc ar (mce2 440 550) 0)
    n = lfNoise0 'α' kr 4
in balance2 s0 s1 n 0.3

-- balance2
let s = sinOsc ar 440 0
    p = sinOsc kr 0.2 0
in balance2 s s p 1 * 0.2

-- balance2
let s = sinOsc ar 440 0
    p = sinOsc kr 0.2 0
in balance2 s s p 0.2

-- balance2
let s0 = sinOsc ar 440 0
    s1 = sinOsc ar 550 0
    x = mouseX kr (-1) 1 Linear 0.2
in balance2 s0 s1 x 0.2

-- balance2
let s = pinkNoise 'α' ar
    l = lpf s 500
    h = s - l
    n = lfNoise2 'β' kr 4
in balance2 l h n 0.1
