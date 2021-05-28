-- redPhasor2 ; no looping & it will play through once ; mouse x acts as trigger
let tr = mouseX KR 0 1 Linear 0.2 `greater_than` 0.5
in sinOsc AR (X.redPhasor2 KR tr 0.3 400 800 0 500 600) 0 * 0.2

-- redPhasor2 ; start value greater than end value, negative rate, c.f. redPhasor
let tr = mouseX KR 0 1 Linear 0.2 `greater_than` 0.5
in sinOsc AR (X.redPhasor2 KR tr (-0.3) 800 400 0 500 600) 0 * 0.2
