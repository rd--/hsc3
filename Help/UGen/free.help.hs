-- free
let n0 = pinkNoise 'α' ar * 0.05
    n1 = dust 'β' kr 1
in mrg [n0,free n1 (-1)]

