> Sound.SC3.UGen.Help.viewSC3Help "PV_ChainUGen.pvcollect"
> :t pvcollect

> import Data.List {- base -}
> import Sound.SC3.ID {- hsc3 -}

> let fileName = "/home/rohan/data/audio/pf-c5.snd"
> in withSC3 (do {_ <- async (b_alloc 10 1024 1)
>                ;async (b_allocRead 11 fileName 0 0)})

> let spectral_delay m p _ =
>     let {l = lfPar KR 0.5 0
>         ;v = linLin l (-1) 1 0.1 1}
>     in (m + delayN m 1 v,p)

> let bpf_sweep nf m p i =
>     let {l = lfPar KR 0.1 0
>         ;e = abs (i - (linLin l (-1) 1 2 (nf / 20)))}
>     in ((e <* 10) * m,p)

> let pv_g nf cf =
>   let {no_op m p _ = (m,p)
>       ;combf m p i = ((modE i 7.0 ==* 0) * m,p)
>       ;sf = playBuf 1 AR 11 (bufRateScale KR 11) 1 0 Loop DoNothing
>       ;c1 = fft' 10 sf
>       ;c2 = pvcollect c1 nf cf 0 250 0}
>   in out 0 (0.1 * ifft' c2)

> let pv_au nf cf =
>   let {no_op m p _ = (m,p)
>       ;combf m p i = ((modE i 7.0 ==* 0) * m,p)
>       ;c1 = fft' 10 (soundIn 4)
>       ;c2 = pvcollect c1 nf cf 0 250 0}
>   in out 0 (0.1 * ifft' c2)

> let r = unlines ["number of constants       : 257"
>                 ,"number of controls        : 0"
>                 ,"control rates             : []"
>                 ,"number of unit generators : 1013"
>                 ,"unit generator rates      : [(KR,5),(AR,4),(DR,1004)]"]
> in r `isPrefixOf` synthstat (pv_g 1024 spectral_delay)

> synthstat (pv_g 1024 (bpf_sweep 1024))
> audition (pv_g 1024 spectral_delay)
> audition (pv_g 1024 (bpf_sweep 1024))

> audition (pv_au 1024 spectral_delay)
> audition (pv_au 1024 (bpf_sweep 1024))

> import Sound.SC3.UGen.Dot {- hsc3-dot -}

> draw_svg (pv_g 1024 (bpf_sweep 1024))
