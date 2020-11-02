-- changed ; simple composition of hpz1 and greater_than
let s = lfNoise0 'α' KR 2
    c = changed s 0
    c' = decay2 c 0.01 0.5
in sinOsc AR (440 + mce2 s c' * 440) 0 * 0.1

-- changed ; sinOsc is constantly changing
let s = sinOsc AR 440 0
    c = changed s 0
in s * c * 0.2
