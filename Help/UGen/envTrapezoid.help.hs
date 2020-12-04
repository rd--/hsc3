-- envTrapezoid
let t = envTrapezoid 0.05 0.95 3 0.1
    e = envGen KR 1 1 0 1 RemoveSynth t
in sinOsc AR 440 0 * e

---- ; drawings
Sound.SC3.Plot.plotEnvelope [envTrapezoid 0.75 0.25 2 1,envTrapezoid 0.25 0.75 3 0.5]

---- ; coord
envelope_sc3_array (envTrapezoid 0 0.25 2 0.1) == Just [0,3,-99,-99,0.1,0.5,1,0,0.1,0,1,0,0,1.5,1,0]
