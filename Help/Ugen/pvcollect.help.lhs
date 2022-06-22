> import Data.List {- base -}
> import Sound.OSC {- hosc -}
> import Sound.Sc3 {- hsc3 -}

> file_name = "/home/rohan/data/audio/instr/bosendorfer/064/C5.aif"

> alloc_buf :: DuplexOSC m => m Message
> alloc_buf = do
>   _ <- async (b_alloc 10 1024 1)
>   async (b_allocRead 11 file_name 0 0)

    > withSc3 alloc_buf

> spectral_delay m p _ =
>   let l = lfPar kr 0.5 0
>       v = linLin l (-1) 1 0.1 1
>   in (m + delayN m 1 v,p)

> bpf_sweep nf m p i =
>   let l = lfPar kr 0.1 0
>       e = abs (constant i - (linLin l (-1) 1 2 (constant nf / 20)))
>   in ((e <** 10) * m,p)

> pv_g nf cf =
>   let no_op m p _ = (m,p)
>       combf m p i = ((modE i 7.0 ==** 0) * m,p)
>       sf = playBuf 1 ar 11 (bufRateScale kr 11) 1 0 Loop DoNothing
>       c1 = fft' 10 sf
>       c2 = pvcollect c1 nf cf 0 250 0
>   in 0.1 * ifft' c2

> pv_au nf cf =
>   let no_op m p _ = (m,p)
>       combf m p i = ((modE i 7.0 ==** 0) * m,p)
>       c1 = fft' 10 (soundIn 0)
>       c2 = pvcollect c1 nf cf 0 250 0
>   in 0.1 * ifft' c2

    > putStrLn $ synthstat_concise (pv_g 1024 spectral_delay)

    > number of constants       : 257
    > number of controls        : 0
    > control rates             : []
    > number of unit generators : 1013
    > unit generator rates      : [(kr,5),(ar,4),(dr,1004)]

   > putStrLn $ synthstat_concise (pv_g 1024 (bpf_sweep 1024))

> g_01 = pv_g 1024 spectral_delay
> g_02 = pv_g 1024 (bpf_sweep 1024)
> g_03 = pv_au 1024 spectral_delay
> g_04 = pv_au 1024 (bpf_sweep 1024)

    > import Sound.Sc3.UGen.Dot {- hsc3-dot -}
    > draw_svg (pv_g 1024 (bpf_sweep 1024))
