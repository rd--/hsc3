    Sound.SC3.UGen.Help.viewSC3Help "StkMandolin"
    Sound.SC3.UGen.DB.ugenSummary "StkMandolin"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as External {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.RDU as RDU {- sc3-rdu -}

> g_01 =
>   let x = mouseX KR 0.25 4 Linear 0.2
>       tr = impulse KR x 0 - 0.5
>       freq = midiCPS (tRand 'α' 54 66 tr)
>       [bs, pp, dm, dt, at] = mceChannels (RDU.tRandN 5 'β' 0 127 tr)
>   in External.stkMandolin AR freq bs pp dm dt at tr

> g_02 =
>   let x = mouseX KR 3 16 Linear 0.2
>       tr = impulse KR x 0 - 0.5 -- trig
>       tr3 = pulseDivider tr 3 0
>       freq = midiCPS (tiRand 'α' 54 66 tr)
>       bs = tRand 'β' 72 94 tr3 -- bodysize
>       pp = tRand 'γ' 32 42 tr3 -- pickposition
>       dm = tRand 'δ' 64 72 tr3 -- stringdamping
>       dt = tRand 'ε' 0 4 tr3 -- stringdetune
>       at = tRand 'ζ' 2 8 tr3 -- aftertouch
>   in External.stkMandolin AR freq bs pp dm dt at tr
