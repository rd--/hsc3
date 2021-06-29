-- sqrDif
(fSinOsc ar 800 0 `sqrDif` fSinOsc ar (xLine kr 200 500 5 DoNothing) 0) * 0.1
