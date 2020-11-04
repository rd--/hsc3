-- mostChange
let n = lfNoise0 'α' KR 1
    x = mouseX KR 200 300 Linear 0.1
    f = mostChange (n * 400 + 900) x
in sinOsc AR f 0 * 0.1
