pv_Copy bufferA bufferB

Copies the spectral frame in bufferA to bufferB at that point in the
chain of PV UGens. This allows for parallel processing of spectral
data without the need for multiple FFT' UGens, and to copy out data at
that point in the chain for other purposes. bufferA and bufferB must
be the same size.

bufferA - source buffer.
bufferB - destination buffer.

Proof of concept, silence

> withSC3 (\fd -> do { async fd (b_alloc 0 2048 1)
>                    ; async fd (b_alloc 1 2048 1) })

> do { i <- lfClipNoise AR 100
>    ; let { c0 = fft' 0 i
>          ; c1 = pv_Copy c0 1 }
>      in audition (out 0 (ifft' c0 - ifft' c1)) }
