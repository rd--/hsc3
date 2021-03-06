-- setResetFF ; d0 is the set trigger, d1 the reset trigger
let d0 = dustId 'β' ar 5
    d1 = dustId 'γ' ar 5
in brownNoiseId 'α' ar * setResetFF d0 d1 * 0.1

-- setResetFF ; silence
let tr = setResetFF (impulse kr 5 0) (impulse kr 10 0)
in brownNoiseId 'α' ar * decay2 tr 0.01 0.05 * 0.1

-- setResetFF ; duty cycle
let tr = 1 - setResetFF (impulse kr 10 0) (impulse kr 5 0)
in brownNoiseId 'α' ar * decay2 tr 0.01 0.05 * 0.1
