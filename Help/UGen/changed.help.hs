-- changed ; simple composition of hpz1 and greater_than
let s = lfNoise0 'α' kr 2
    c = changed s 0
    c' = decay2 c 0.01 0.5
in sinOsc ar (440 + mce2 s c' * 440) 0 * 0.1

-- changed ; sinOsc is constantly changing
let s = sinOsc ar 440 0
    c = changed s 0
in s * c * 0.2
