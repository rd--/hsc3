-- lfBrownNoise2
let freq = 1000
    dev = mouseX kr 0 1 Linear 0.2
    dist = mouseY kr 0 5 Linear 0.2
in X.lfBrownNoise2Id 'α' ar freq dev dist * 0.25

-- lfBrownNoise2 ; as frequency control
let freq = 8
    dev = mouseX kr 0 1 Linear 0.2
    dist = mouseY kr 0 5 Linear 0.2
    n1:n2:n3:_ = map (\z -> X.lfBrownNoise2Id z kr freq dev dist) ['α'..]
    o = impulse ar (range 6 24 n1) 0
in lagUD o (range 0.0001 0.001 n2) (range 0.0001 0.001 n3) * 0.5

-- lfBrownNoise2 ; as pan & volume controls ; warning=feedback
let s = soundIn 0
    freq = range 0.5 2 (X.lfBrownNoise2Id 'α' kr 2 0.1 5)
    dev = mouseX kr 0.01 0.35 Linear 0.2
    dist = mouseY kr 0 5 Linear 0.2
    n1:n2:_ = map (\z -> X.lfBrownNoise2Id z kr freq dev dist) ['β'..]
in pan2 s (range (-0.75) 0.75 n1) 1 * range 0.01 0.5 n2
