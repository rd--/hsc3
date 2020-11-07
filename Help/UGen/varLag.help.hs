-- varLag ; the varLag UGen is not working ; there is varLag_env but it is also not working
let sqr = linLin (lfPulse AR 1 0 0.5) 0 1 100 400
    crv = varLag_env sqr 0.2 (EnvNum (line KR (-8) 8 15 RemoveSynth)) 0
in sinOsc AR crv 0 * 0.3
