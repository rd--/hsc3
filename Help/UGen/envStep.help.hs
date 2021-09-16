-- envStep
let env = envStep [0.0,0.5,0.7,1.0,0.9,0.0] [0.5,0.1,0.2,1.0,1.5,3] Nothing Nothing
    envgen = envGen ar 1 1 0 1 RemoveSynth env
in sinOsc ar (envgen * 1000 + 440) 0 * (envgen + 1) * 0.1

-- envStep ; major scale, accelerating, with loop & release nodes
let env = envStep [0,2,4,5,7,9,11,12,0] (take 9 (iterate (* 0.75) 1)) (Just 8) (Just 0)
    envgen = envGen ar 1 1 0 1 DoNothing env
in sinOsc ar (midiCps (envgen + 60)) 0 * 0.1

---- ; drawings
Sound.SC3.Plot.plotEnvelope [envStep [0,2,4,5,7,9,11,12] (take 8 (iterate (* 0.75) 1)) Nothing Nothing]

---- ; help
Sound.SC3.Lang.Help.viewSC3Help "Env.*step"

