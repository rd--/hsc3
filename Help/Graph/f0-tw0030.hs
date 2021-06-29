-- http://www.fredrikolofsson.com/f0blog/?q=node/537 (f0)
let a i j k l = lfPar ar i j * k + l
    f = a 1 0 5 (a (mce2 0.05 0.04) 0 50 160 `roundTo` 50)
    w = a 0.2 0 0.5 (a 3 0 0.2 0.5)
    o = varSaw ar f 0 w / 8
in gVerb o 80 3 0.5 0.5 15 1 0.7 0.5 300 * 0.1
