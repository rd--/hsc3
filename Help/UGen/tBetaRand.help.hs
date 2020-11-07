-- tBetaRand
let t = dust 'α' KR 10
    f = X.tBetaRand 'β' 300 3000 0.1 0.1 t
in sinOsc AR f 0 * 0.1

---- ; ERROR ; tBetaRand ; mouse control of parameters (...scsynth -> 100% CPU -> CRASH)
let t = dust 'α' AR 10
    p1 = mouseX KR 1 5 Linear 0.2
    p2 = mouseY KR 1 5 Linear 0.2
    f = X.tBetaRand 'β' 300 3000 p1 p2 t
in sinOsc AR f 0 * 0.1

---- ; ERROR ; audio rate crashes server
let t = dust 'α' AR 100
    p1 = mouseX KR 1 5 Linear 0.2
    p2 = mouseY KR 1 5 Linear 0.2
in lag (X.tBetaRand 'β' (-1) 1 p1 p2 t) (10 / 48000)
