-- bufAllpassL ; filtered decaying noise bursts
let b = localBufId 'α' 1 44100
    d = dustId 'β' ar 1
    n = whiteNoiseId 'γ' ar
    x = decay d 0.2 * n * 0.25
in bufAllpassL b x 0.25 6
