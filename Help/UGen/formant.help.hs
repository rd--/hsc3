-- formant ; default values
formant AR 440 1760 880 * 0.125

-- formant ; modulate fundamental frequency, formant frequency stays constant
formant AR (xLine KR 400 1000 8 RemoveSynth) 2000 800 * 0.125

-- formant ; modulate formant frequency, fundamental frequency stays constant
let f = mce [200, 300, 400, 500]
    ff = xLine KR 400 4000 8 RemoveSynth
in formant AR f ff 200 * 0.125

-- formant ; modulate width frequency, other frequencies stay constant
let bw = xLine KR 800 8000 8 RemoveSynth
in formant AR 400 2000 bw * 0.1
