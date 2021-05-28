-- redPhasor ; no looping & it will play through once ; mouse x acts as trigger
let tr = mouseX KR 0 1 Linear 0.2 `greater_than` 0.5
in sinOsc AR (X.redPhasor KR tr 0.3 400 800 0 500 600) 0 * 0.2

-- redPhasor ; start value greater than end value, positive rate, c.f. redPhasor2
let tr = mouseX KR 0 1 Linear 0.2 `greater_than` 0.5
in sinOsc AR (X.redPhasor KR tr 0.3 800 400 0 500 600) 0 * 0.2

-- redPhasor ; mouse y controls looping on/off, mouse x trigger
let tr = mouseX KR 0 1 Linear 0.2 `greater_than` 0.5
    lp = mouseY KR 0 1 Linear 0.2 `greater_than` 0.5
in sinOsc AR (X.redPhasor KR tr 0.3 400 800 lp 500 600) 0 * 0.2

-- redPhasor ; mouse x controls loop rate, mouse y scales the start loop-point
let x = mouseX KR 0 5 Linear 0.2
    y = mouseY KR 200 500 Linear 0.2
in sinOsc AR (X.redPhasor KR 0 x 400 800 1 y 600) 0 * 0.2
