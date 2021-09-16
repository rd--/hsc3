-- demanding studies (jmcc)
let s1 = drand dinf (mce [72, 75, 79, 82])
    s2 = drand 1 (mce [82, 84, 86])
    s3 = dseq dinf (mce [72, 75, 79, s2])
    x = mouseX kr 5 13 Linear 0.2
    tr = impulse kr x 0
    f = demand tr 0 (mce2 (midiCps (s1 - 12)) (midiCps s3))
    o1 = sinOsc ar (f + mce2 0 0.7) 0
    o2 = saw ar (f + mce2 0 0.7) * 0.3
    o3 = cubed (distort (log (distort (o1 + o2))))
in o3 * 0.1

-- demanding studies (jmcc) ; id
let s1 = drandId 'α' dinf (mce [72, 75, 79, 82])
    s2 = drandId 'β' 1 (mce [82, 84, 86])
    s3 = dseqId 'γ' dinf (mce [72, 75, 79, s2])
    x = mouseX kr 5 13 Linear 0.2
    tr = impulse kr x 0
    f = demand tr 0 (mce2 (midiCps (s1 - 12)) (midiCps s3))
    o1 = sinOsc ar (f + mce2 0 0.7) 0
    o2 = saw ar (f + mce2 0 0.7) * 0.3
    o3 = cubed (distort (log (distort (o1 + o2))))
in o3 * 0.1
