-- schmidt ; threshold octave jumps
let n = lfNoise1 'α' KR 3
    o = schmidt n (-0.15) 0.15 + 1
in sinOsc AR (n * 200 + 500 * o) 0 * 0.1
