-- gcd
let x = mouseX kr (-200) 200 Linear 0.2
    y = mouseY kr (-200) 200 Linear 0.2
in sinOsc ar ((sinOsc kr 0.3 0 * 20) `gcdE` mce2 x y * 30 + 500) 0 * 0.1

-- https://www.listarc.bham.ac.uk/lists/sc-users/msg68916.html (jrhb)
let a = lfSaw ar 0.02 0 * 300 + 1
    b = lfSaw ar (1.24 + mce2 0 0.1) 0 * 200 + 1
in sinOsc ar (gcdE a b * 30 + 300) 0 * 0.1

-- https://www.listarc.bham.ac.uk/lists/sc-users/msg68916.html (jrhb)
let a = lfSaw ar 0.012 0 * 300 + 1
    b = lfSaw ar (0.24 + mce2 0 0.01) 0 * 200 + 1
in sinOsc ar (gcdE a b * 30 + 300) 0 * 0.1

-- https://www.listarc.bham.ac.uk/lists/sc-users/msg68916.html (jrhb)
let a = lfSaw kr 0.002 0 * 300 + 1
    b = lfSaw kr (0.024 + mce2 0 0.001) 0 * 200 + 1
in sinOsc ar (gcdE a b * 43 + 300) 0 * 0.1
