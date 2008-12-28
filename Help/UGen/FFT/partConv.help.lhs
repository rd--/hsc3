partConv in fft_size ir_bufnum

Partitioned convolution. Various additional buffers
must be supplied.

Mono impulse response only! If inputting multiple
channels, you'll need independent PartConvs, one
for each channel.

But the charm is: impulse response can be as large
as you like (CPU load increases with IR
size. Various tradeoffs based on fftsize choice,
due to rarer but larger FFTs. This plug-in uses
amortisation to spread processing and avoid
spikes).

Normalisation factors difficult to anticipate;
convolution piles up multiple copies of the input
on top of itself, so can easily overload.

         in - processing target

    fftsize - spectral convolution partition size
              (twice partition size). You must
              ensure that the blocksize divides the
              partition size and there are at least
              two blocks per partition (to allow
              for amortisation)

   irbufnum - Prepared buffer of spectra for each 
              partition of the impulse response

preparation; essentially, allocate an impulse
response buffer, then follow some special buffer
preparation steps below to set up the data the
plugin needs. 

> import Sound.SC3

> let { fft_size = 2048
>     ; ir_file = "/home/rohan/audio/church.ir.wav"
>     ; ir_length = 82756
>     ; accum_size = pc_calcAccumSize fft_size ir_length
>     ; ir_td_b = 10 {- time domain -}
>     ; ir_fd_b = 11 {- frequency domain -}
>     ; target_b = 12 {- source signal -}
>     ; target_file = "/home/rohan/audio/text.snd"
>     ; c = constant
>     ; g = let { i = playBuf 1 (c target_b) 1 0 0 Loop DoNothing
>               ; pc = partConv i (c fft_size) (c ir_fd_b) }
>           in out 0 (pc * 0.1) }
> in withSC3 (\fd -> do 
>     { async fd (b_allocRead ir_td_b ir_file 0 ir_length)
>     ; async fd (b_alloc ir_fd_b accum_size 1)
>     ; send fd (pc_preparePartConv ir_fd_b ir_td_b fft_size)
>     ; async fd (b_allocRead target_b target_file 0 0)
>     ; play fd g })
