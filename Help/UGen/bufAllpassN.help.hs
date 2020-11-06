-- bufAllpassN ; filtered decaying noise bursts
let b = localBuf 'α' 1 44100
    d = dust 'β' AR 1
    n = whiteNoise 'γ' AR
    x = decay d 0.2 * n * 0.25
in bufAllpassN b x 0.25 6
