    Sound.SC3.UGen.Help.viewSC3Help "StkModalBar"
    Sound.SC3.UGen.DB.ugenSummary "StkModalBar"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as External {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.RDU as RDU {- sc3-rdu -}

> g_01 =
>   let x = mouseX KR 0.25 12 Linear 0.2
>       tr = impulse KR x 0 - 0.5
>       tR = tRand 'α' 0 127 tr
>       i = tRand 'β' 0 9 tr
>       mn = tiRand 'γ' 25 96 tr
>       [sh,sp,vg,vf,mx,v] = mceChannels (RDU.tRandN 6 'δ' 0 127 tr)
>   in External.stkModalBar AR (midiCPS mn) i sh sp vg vf mx v tr

> g_02 =
>   let x = mouseX KR 1 12 Linear 0.2
>       tr = impulse KR x 0 - 0.5
>       tr3 = pulseDivider tr 3 0
>       freq = midiCPS (tiRand 'α' 52 64 tr)
>       instr = 1 -- instrument (0 - 9)
>       sh = tRand 'β' 10 50 tr3 -- stickhardness
>       sp = tRand 'γ' 40 80 tr3 -- stickposition
>       vg = tRand 'δ' 66 98 tr3 -- vibratogain
>       vf = tRand 'ε' 4 12 tr3 -- vibratofreq
>       mx = tRand 'ζ' 0 1 tr3 -- directstickmix
>       v = tRand 'η' 16 48 tr3 -- volume
>   in External.stkModalBar AR freq instr sh sp vg vf mx v tr

<https://ccrma.stanford.edu/software/stk/classstk_1_1ModalBar.html>
