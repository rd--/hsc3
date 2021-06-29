-- bufAllpassL ; filtered decaying noise bursts
let b = localBuf 'α' 1 44100
    d = dust 'β' ar 1
    n = whiteNoise 'γ' ar
    x = decay d 0.2 * n * 0.25
in bufAllpassL b x 0.25 6
