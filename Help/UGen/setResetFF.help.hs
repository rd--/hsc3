-- setResetFF ; d0 is the set trigger, d1 the reset trigger
let d0 = dust 'β' AR 5
    d1 = dust 'γ' AR 5
in brownNoise 'α' AR * setResetFF d0 d1 * 0.1

-- setResetFF ; silence
let tr = setResetFF (impulse KR 5 0) (impulse KR 10 0)
in brownNoise 'α' AR * decay2 tr 0.01 0.05 * 0.1

-- setResetFF ; duty cycle
let tr = 1 - setResetFF (impulse KR 10 0) (impulse KR 5 0)
in brownNoise 'α' AR * decay2 tr 0.01 0.05 * 0.1
