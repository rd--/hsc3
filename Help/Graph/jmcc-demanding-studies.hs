-- demanding studies (jmcc)
let s1 = drand 'α' dinf (mce [72, 75, 79, 82])
    s2 = drand 'β' 1 (mce [82, 84, 86])
    s3 = dseq 'γ' dinf (mce [72, 75, 79, s2])
    x = mouseX KR 5 13 Linear 0.2
    tr = impulse KR x 0
    f = demand tr 0 (mce2 (midiCPS (s1 - 12)) (midiCPS s3))
    o1 = sinOsc AR (f + mce2 0 0.7) 0
    o2 = saw AR (f + mce2 0 0.7) * 0.3
    o3 = cubed (distort (log (distort (o1 + o2))))
in o3 * 0.1
