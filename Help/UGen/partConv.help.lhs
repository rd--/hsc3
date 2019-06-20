> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> f_01 :: Transport m => m UGen
> f_01 = do
>   let target_b = 12 {- source signal -}
>       target_file = "/home/rohan/data/audio/pf-c5.snd"
>   _ <- async (b_allocRead target_b target_file 0 0)
>   return (playBuf 1 AR (constant target_b) 1 0 0 Loop DoNothing)

> f_02 :: Transport m => UGen -> m UGen
> f_02 s = do
>   let fft_size = 2048
>       ir_file = "/home/rohan/data/audio/reverbs/chapel.wav"
>       ir_length = 62494 {- frame count of ir_file -}
>       accum_size = pc_calcAccumSize fft_size ir_length
>       ir_td_b = 10 {- time domain -}
>       ir_fd_b = 11 {- frequency domain -}
>   _ <- async (b_allocRead ir_td_b ir_file 0 ir_length)
>   _ <- async (b_alloc ir_fd_b accum_size 1)
>   sendMessage (pc_preparePartConv ir_fd_b ir_td_b fft_size)
>   return (partConv s (constant fft_size) (constant ir_fd_b))

    g_01 <- withSC3 f_01
    g_02 <- withSC3 (f_02 (g_01 * 0.1)
    g_03 <- withSC3 (f_02 (soundIn 0))
