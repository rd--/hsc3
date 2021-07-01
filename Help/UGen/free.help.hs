-- free
let n0 = pinkNoiseId 'α' ar * 0.05
    n1 = dustId 'β' kr 1
in mrg [n0,free n1 (-1)]

