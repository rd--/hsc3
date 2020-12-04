-- free
let n0 = pinkNoise 'α' AR * 0.05
    n1 = dust 'β' KR 1
in mrg [n0,free n1 (-1)]

