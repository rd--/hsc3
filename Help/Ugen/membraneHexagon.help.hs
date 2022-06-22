-- membraneHexagon ; mouse control
let mb = mouseButton kr 0 1 0.2
    mx = mouseX kr 0.01 0.1 Linear 0.2
    my = mouseY kr 0.999999 0.999 Exponential 0.2
    ex = pinkNoiseId 'α' ar * envGen kr mb 0.4 0 0.1 DoNothing (envPerc 0.01 1)
in X.membraneHexagon ar ex mx my

-- membraneHexagon ; event control
let f (_,g,x,y,z,o,rx,_,_,_,_) =
      let ex = pinkNoiseId 'α' ar * envGen kr g z 0 0.1 DoNothing (envPerc (0.02 * rx) 1)
          tn = x * 0.1
          ls = linExp y 0 1 0.999999 0.9999
      in pan2 (X.membraneHexagon ar ex tn ls) (o * 2 - 1) 1
in mix (eventVoicer 8 f) * control kr "gain" 1
