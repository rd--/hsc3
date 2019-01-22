    Sound.SC3.UGen.Help.viewSC3Help "StkBandedWG"
    Sound.SC3.UGen.DB.ugenSummary "StkBandedWG"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Record.Plain.StkBandedWG {- hsc3-rec -}

> g_01 = mkStkBandedWG stkBandedWGR

instr: Uniform Bar = 0, Tuned Bar = 1, Glass Harmonica = 2, Tibetan Bowl = 3

> g_02 = mkStkBandedWG stkBandedWGR {instr = 3}

> g_03 =
>   let tr = impulse KR (mouseX KR 0.5 12 Linear 0.2) 0 - 0.5
>       u = stkBandedWGR
>           {freq = midiCPS (tExpRand 'α' 24 96 tr)
>           ,modalresonance = tRand 'ζ' 0 127 tr
>           ,instr = 3
>           ,trig_ = tr}
>   in mkStkBandedWG u

setstriking: 0 = Plucked, 127 = Bowed

> g_04 =
>   let u = stkBandedWGR
>           {freq = rand 'α' 110 440
>           ,instr = iRand 'β' 1 3
>           ,bowpressure = iRand 'γ' 32 96
>           ,bowmotion = rand 'δ' 32 96
>           ,integration = rand 'ε' 0 64
>           ,modalresonance = rand 'ζ' 32 96
>           ,bowvelocity = rand 'η' 64 96
>           ,setstriking = 127
>           ,trig_ = 1}
>   in mkStkBandedWG u
