-- schmidt ; threshold octave jumps
let n = lfNoise1Id 'α' kr 3
    o = schmidt n (-0.15) 0.15 + 1
in sinOsc ar (n * 200 + 500 * o) 0 * 0.1
