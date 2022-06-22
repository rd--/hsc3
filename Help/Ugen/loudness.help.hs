-- loudness ; assume hop-size of half fft-size
let b = localBufId 'α' 1 1024
    x = mouseX kr 0.001 0.1 Exponential 0.2
    i = sinOsc ar 1000 0 * x
    f = fft' b i
    l = loudness f 0.25 6
in sinOsc ar (mce2 900 (l * 300 + 600)) 0 * 0.1
