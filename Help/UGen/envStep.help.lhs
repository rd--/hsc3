    Sound.SC3.UGen.Help.viewSC3Help "Env.*step"
    :i Sound.SC3.envStep

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let env = envStep [0.0,0.5,0.7,1.0,0.9,0.0] [0.5,0.1,0.2,1.0,1.5,3] Nothing Nothing
>         envgen = envGen AR 1 1 0 1 RemoveSynth env
>     in sinOsc AR (envgen * 1000 + 440) 0 * (envgen + 1) * 0.1

major scale, accelerating, with loop & release nodes

> g_02 =
>     let env = envStep [0,2,4,5,7,9,11,12,0] (take 9 (iterate (* 0.75) 1)) (Just 8) (Just 0)
>         envgen = envGen AR 1 1 0 1 DoNothing env
>     in sinOsc AR (midiCPS (envgen + 60)) 0 * 0.1

draw envelope

> e_01 = [envStep [0,2,4,5,7,9,11,12] (take 8 (iterate (* 0.75) 1)) Nothing Nothing]

    import Sound.SC3.Plot
    plotEnvelope e_01
