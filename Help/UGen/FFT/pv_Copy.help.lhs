pv_Copy bufferA bufferB

Copies the spectral frame in bufferA to bufferB at that point in the
chain of PV UGens. This allows for parallel processing of spectral
data without the need for multiple FFT' UGens, and to copy out data at
that point in the chain for other purposes. bufferA and bufferB must
be the same size.

bufferA - source buffer.
bufferB - destination buffer.

Proof of concept, silence

> withSC3 (\fd -> do send fd (b_alloc 0 2048 1)
>                    wait fd "/done"
>                    send fd (b_alloc 1 2048 1)
>                    wait fd "/done")
> i <- lfClipNoise AR 100
> let c0 = fft' 0 i
>     c1 = pv_Copy c0 1
> audition (out 0 (ifft' c0 - ifft' c1))
