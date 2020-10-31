-- lcm
let x = mouseX KR (-20) 20 Linear 0.2
    y = mouseY KR (-20) 20 Linear 0.2
in sinOsc AR ((sinOsc KR 0.3 0 * 20) `lcmE` mce2 x y * 30 + 500) 0 * 0.1

