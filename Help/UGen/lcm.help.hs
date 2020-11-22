-- lcm
let x = mouseX KR (-20) 20 Linear 0.2
    y = mouseY KR (-20) 20 Linear 0.2
in sinOsc AR ((sinOsc KR 0.3 0 * 20) `lcmE` mce2 x y * 30 + 500) 0 * 0.1

-- https://www.listarc.bham.ac.uk/lists/sc-users/msg68916.html (jrhb)
let a = lfSaw AR 2 0 * 100
    b = lfSaw AR 1.2 0 * 100
in sinOsc AR (lcmE a b + 300) 0 * 0.1

