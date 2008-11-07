LocalBuf id nf nc m

    nf - number of frames (default: 1)
    nc - number of channels for multiple channel buffers (default: 1)
     m - the first LocalBuf in a SynthDef may define the maximum buffers 
         used (default 8)

Allocate a buffer local to the synthesis graph.

> import Sound.SC3

> do { n <- whiteNoise AR
>    ; let { b = localBuf (uid 0) 2048 1 8
>          ; f = fft' b n
>          ; c = pv_BrickWall f (sinOsc KR 0.1 0 * 0.75) }
>      in audition (out 0 (ifft' c * 0.1)) }

> do { n <- clone 2 (whiteNoise AR)
>    ; let { b = mce (map (\i -> localBuf (uid i) 2048 1 8) [0, 1])
>          ; f = fft' b n
>          ; c = pv_BrickWall f (sinOsc KR (mce2 0.1 0.11) 0 * 0.75) }
>      in audition (out 0 (ifft' c * 0.1)) }

not clearing the buffer accesses old data:
slowly overwrite data with noise

> let { b = localBuf (uid 0) 2048 2 8
>     ; nf = bufFrames KR b
>     ; x = mouseX KR 1 2 Linear 0.2
>     ; r = playBuf 2 b x 1 0 Loop * 0.1
>     ; wr ph i = bufWr b (linLin ph (-1) 1 0 nf) Loop i }
> in do { n <- clone 2 (whiteNoise AR)
>       ; ph <- lfNoise0 AR 530
>       ; audition (mrg2 (out 0 r) (wr ph n)) }

bufCombC needs no clearing, because the delay line is filled by the ugen

> do { d <- clone 2 (dust AR 1)
>    ; n <- whiteNoise AR
>    ; let { z = decay d 0.3 * n
>          ; l = xLine KR 0.0001 0.01 20 DoNothing
>          ; sr = sampleRate 
>          ; b = mce (map (\i -> localBuf (uid i) sr 2 8) [0, 1]) }
>      in audition (out 0 (bufCombC b z l 0.2)) }

asLocalBuf combines localBuf and setBuf

> let { b = asLocalBuf (uid 0) [2, 1, 5, 3, 4, 0]
>     ; x = mouseX KR 0 (bufFrames KR b) Linear 0.2
>     ; f = indexL b x * 100 + 40
>     ; o = saw AR (f * mce2 1 1.1) * 0.1 }
> in audition (out 0 o)

> let { b = asLocalBuf (uid 0) [2, 3, 4, 0, 1, 5]
>     ; n = bufFrames KR b
>     ; x = floorE (mouseX KR 0 n Linear 0.1)
>     ; i = detectIndex b x }
> in audition (out 0 (sinOsc AR (linExp i 0 n 200 700) 0 * 0.1))

> do { n <- lfNoise1 KR (mce [3, 3.05])
>    ; let { x = mouseX KR 0 15 Linear 0.1
>          ; b = asLocalBuf (uid 0) [0, 2, 3.2, 5, 7, 9, 10]
>          ; k = degreeToKey b x 12
>          ; f b = let { o = sinOsc AR (midiCPS (b + k + n * 0.04)) 0 * 0.1
>                      ; t = lfPulse AR (midiCPS (mce [48, 55])) 0.15 0.5
>                      ; d = rlpf t (midiCPS (sinOsc KR 0.1 0 * 10 + b)) 0.1 * 0.1
>                      ; m = o + d }
>                  in combN m 0.31 0.31 2 + m }
>      in audition (out 0 ((f 48 + f 72) * 0.25)) }
