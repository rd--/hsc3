-- weaklyNonlinear2 ; mouseY controls input strength of forcing oscillator
let input = saw ar 261.626 * mouseY kr 0.0001 1 Exponential 0.1
    freq = mouseX kr 100 400 Linear 0.1
in pan2 (X.weaklyNonlinear2 ar input 0 1 1 freq 0 0 0 0 0 0) 0 0.1

-- weaklyNonlinear2 ; van der Pol equation
let input = sinOsc ar (mouseX kr 10 2000 Linear 0.1) 0 * 0.1
    reset = impulse kr (mouseY kr 0 100 Linear 0.1) 0
in pan2 (X.weaklyNonlinear2 ar input reset 1 1 440 0 0 (-0.01) 2 (-1) 1) 0 0.1

-- weaklyNonlinear2 ; duffing equation
let input = sinOsc ar (mouseX kr 1 1000 Exponential 0.1) 0 * 0.1
    freq = mouseY kr 1 1000 Exponential 0.1
in pan2 (X.weaklyNonlinear2 ar input 0 1 1 freq 0 0 (-0.001) 3 0 0) 0 0.1

-- weaklyNonlinear2 ; event control
let f _ (g,x,y,z,o,_,_,_,_,_) =
      let input = saw ar 261.626 * linExp y 0 1 0.0001 1
          freq = x * 300 + 100
      in pan2 (X.weaklyNonlinear2 ar input 0 1 1 freq 0 0 0 0 0 0) (o * 2 - 1) (z * g)
in mix (eventVoicer 16 f) * control kr "gain" 0.5
