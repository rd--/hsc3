pv_BinWipe bufferA bufferB wipe

Combine low and high bins from two inputs.  Copies low bins from one
input and the high bins of the other.

bufferA - fft buffer A.
bufferB - fft buffer B.
wipe    - can range between -1 and +1.

if wipe == 0 then the output is the same as inA.
if  wipe > 0 then it begins replacing with bins from inB from the bottom up.
if  wipe < 0 then it begins replacing with bins from inB from the top down.

> let { fileName = "/home/rohan/audio/metal.wav"
>     ; async h m = send h m >> wait h "/done" }
> in withSC3 (\fd -> do { async fd (b_alloc 10 2048 1)
>                       ; async fd (b_alloc 11 2048 1)
>                       ; async fd (b_allocRead 12 fileName 0 0) })

> do { n <- whiteNoise AR
>    ; let { b = playBuf 1 12 (bufRateScale KR 12) 0 0 Loop
>          ; f = fft' 10 (n * 0.2)
>          ; g = fft' 11 b
>          ; x = mouseX KR 0.0 1.0 Linear 0.1
>          ; h = pv_BinWipe f g x }
>      in audition (out 0 (pan2 (ifft' h) 0 0.5)) }
