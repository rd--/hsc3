-- lcm
let x = mouseX kr (-20) 20 Linear 0.2
    y = mouseY kr (-20) 20 Linear 0.2
in sinOsc ar ((sinOsc kr 0.3 0 * 20) `lcmE` mce2 x y * 30 + 500) 0 * 0.1

-- https://www.listarc.bham.ac.uk/lists/sc-users/msg68916.html (jrhb)
let a = lfSaw ar 2 0 * 100
    b = lfSaw ar 1.2 0 * 100
in sinOsc ar (lcmE a b + 300) 0 * 0.1

