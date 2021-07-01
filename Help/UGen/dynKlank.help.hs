-- dynKlank
let s = klankSpec [800,1071,1153,1723] [1,1,1,1] [1,1,1,1]
in dynKlank (impulse ar 2 0 * 0.1) 1 0 1 s

-- dynKlank
let s = klankSpec [800,1071,1353,1723] [1,1,1,1] [1,1,1,1]
in dynKlank (dustId 'α' ar 8 * 0.1) 1 0 1 s

-- dynKlank
let s = klankSpec [800,1071,1353,1723] [1,1,1,1] [1,1,1,1]
in dynKlank (pinkNoiseId 'α' ar * 0.007) 1 0 1 s

-- dynKlank
let s = klankSpec [200,671,1153,1723] [1,1,1,1] [1,1,1,1]
in dynKlank (pinkNoiseId 'α' ar * 0.004) 1 0 1 s

-- dynKlank ; change frequencies (x) and ring-times (y) with mouse.
let x = mouseX kr 0.5 2 Exponential 0.2
    f = map (* x) [800,1071,1153,1723]
    y = mouseY kr 0.1 10 Exponential 0.2
    d = map (* y) [1,1,1,1]
    s = klankSpec f [1,1,1,1] d
    i = impulse ar 2 0 * 0.1
in dynKlank i 1 0 1 s
