-- tBetaRand
let t = dustId 'α' kr 10
    f = X.tBetaRandId 'β' 300 3000 0.1 0.1 t
in sinOsc ar f 0 * 0.1

---- ; ERROR ; tBetaRand ; mouse control of parameters (...scsynth -> 100% CPU -> CRASH)
let t = dustId 'α' ar 10
    p1 = mouseX kr 1 5 Linear 0.2
    p2 = mouseY kr 1 5 Linear 0.2
    f = X.tBetaRandId 'β' 300 3000 p1 p2 t
in sinOsc ar f 0 * 0.1

---- ; ERROR ; audio rate crashes server
let t = dustId 'α' ar 100
    p1 = mouseX kr 1 5 Linear 0.2
    p2 = mouseY kr 1 5 Linear 0.2
in lag (X.tBetaRandId 'β' (-1) 1 p1 p2 t) (10 / 48000)
