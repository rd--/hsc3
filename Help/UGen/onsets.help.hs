-- onsets ; x varies threshold ; detector triggers new random oscillator frequency
let x = mouseX KR 0 1 Linear 0.2
    i = mix (soundIn (mce2 0 1))
    c = fft' (localBuf 'γ' 1 512) i
    o = onsets' c x (onsetType "rcomplex")
in sinOsc AR (midiCPS (tiRand 'a' 40 72 o)) 0 * 0.1

-- onsets ; generative signal with distinct onsets ; x varies threshold ; detector triggers noise
let e = linLin (saw AR 2) (-1) 1 0 1
    p = let f = midiCPS (tiRand 'α' 63 75 (impulse KR 2 0)) in pulse AR f 0.5 * 0.1
    f = linExp (lfNoise2 'β' KR 0.5) (-1) 1 100 10000
    z = lpf p f * e
    c = fft' (localBuf 'γ' 1 512) z
    x = mouseX KR 0 1 Linear 0.2
    o = onsets' c x (onsetType "rcomplex")
    n = let d = envPerc 0.001 0.1 in whiteNoise 'δ' AR * envGen KR o 0.2 0 1 DoNothing d
in pan2 z (-0.75) 0.2 + pan2 n 0.75 1
