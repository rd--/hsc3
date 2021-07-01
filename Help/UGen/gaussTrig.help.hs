-- gaussTrig
let x = mouseX kr 0 0.9 Linear 0.2
    t1 = X.gaussTrig kr 10 x * abs (whiteNoiseId 'α' kr) * 0.5
    t2 = dustId 'β' kr 10 * 0.5
    n = pinkNoiseId 'γ' ar * decay (mce2 t1 t2) 0.02 * 0.5
in fold2 (ringz n 2000 0.02) 0.5
