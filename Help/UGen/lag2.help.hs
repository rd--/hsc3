-- lag2 ; control
let x = mouseX kr 220 440 Exponential 0.1
in sinOsc ar (mce [x, lag2 x 1]) 0 * 0.1

-- lag2 ; audio
lag2 (impulse ar 100 0) (mouseX kr 0.0 0.01 Linear 0.2)
