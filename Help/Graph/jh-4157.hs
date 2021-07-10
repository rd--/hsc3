-- jh ; <https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157>
mix (sinOsc ar (300 * mce (map (4 **) (take 200 [0, 0.002 ..]))) 0 * 0.02)

-- jh ; <https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157> ; doppler
splay (sinOsc ar (120 * mce (map (2 **) (take 100 [0, 0.002 ..]))) 0) 1 0.1 0 True

-- jh ; <https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157> ; doppler-formants
splay (saw ar (120 * mce (map (2 **) (take 100 [0, 0.002 ..])))) 1 0.1 0 True

-- sinosc ; <https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157> ; rd (edit)
let k = 100 -- 200 ; c.f. 12 25 50 100 200 400 600 800
    b = 2 -- 4 ; c.f. 2, 3, 4
    n = 0.02 -- 0.002 ; c.f. 0.002 0.004 0.01 0.05 0.1 0.125
    f0 = 120 -- 300 ; c.f. 120 180 240
    fMul = map (b **) (take k [0, n ..])
in splay (sinOsc ar (mce fMul * f0) 0) 1 0.1 0 True

-- sinosc ; <https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157> ; rd (edit)
let k = 160
    t = impulse ar 0.1 0
    sinosc f = sin (phasor ar t (f * sampleDur) 0 1 0 * 2 * pi)
    b = tRand 2 2.25 t
    n = tRand 0.002 0.02 t
    f0 = tRand 90 180 t
    fMul = map (b **) (take k [0, n ..])
    e = envGen ar t 1 0 1 DoNothing (envPerc 1 10)
in splay (sinosc (mce fMul * f0)) 1 0.1 0 True * e
