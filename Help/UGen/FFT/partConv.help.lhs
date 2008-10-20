partConv in fft_size ir_bufnum accum_bufnum

Partitioned convolution. Various additional buffers
must be supplied.

Mono impulse response only! If inputting multiple
channels, you'll need independent accumulation
buffers for each channel.

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

accumbufnum - Accumulation buffer is a storage
              space for spectral accumulation of
              fft data from partitions; must be
              same saize as irbuffer

preparation; essentially, allocate an impulse
response buffer, then follow some special buffer
preparation steps below to set up the data the
plugin needs. Different options are provided
commented out for loading impulse responses from
soundfiles.

> let { fft_size = 2048
>     ; ir_file = "/home/rohan/audio/church.ir.wav"
>     ; ir_length = 82756
>     ; accum_size = pc_calcAccumSize fft_size ir_length
>     ; ir_td_b = 10 {- time domain -}
>     ; ir_fd_b = 11 {- frequency domain -}
>     ; accum_b = 12  {- internal accumulator -}
>     ; target_b = 13 {- source signal -}
>     ; target_file = "/home/rohan/audio/text.snd"
>     ; c = constant
>     ; g = let { i = playBuf 1 (c target_b) 1 0 0 Loop
>               ; pc = partConv i (c fft_size) (c ir_fd_b) (c accum_b) }
>           in out 0 (pc / constant accum_size) }
> in withSC3 (\fd -> do 
>     { async fd (b_allocRead ir_td_b ir_file 0 ir_length)
>     ; async fd (b_alloc ir_fd_b accum_size 1)
>     ; async fd (b_alloc accum_b accum_size 1)
>     ; send fd (pc_preparePartConv ir_fd_b ir_td_b fft_size)
>     ; async fd (b_allocRead target_b target_file 0 0)
>     ; play fd g })
