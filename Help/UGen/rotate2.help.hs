-- rotate2 ; rotation of stereo sound, via LFO
let x = pinkNoise 'α' AR
    y = lfTri AR 800 0 * lfPulse KR 3 0 0.3
in rotate2 x y (lfSaw KR 0.1 0) * 0.1

-- rotate2 ; rotation of stereo sound, via mouse
let x = mix (lfSaw AR (mce [198..201]) 0) * 0.25
    y = sinOsc AR 900 0 * lfPulse KR 3 0 0.3
    p = mouseX KR 0 2 Linear 0.2
in rotate2 x y p * 0.1
