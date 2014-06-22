> Sound.SC3.UGen.Help.viewSC3Help "LocalBuf"
> Sound.SC3.UGen.DB.ugenSummary "LocalBuf"

> import Sound.SC3.ID

Allocate a buffer local to the synthesis graph.

> let {n = whiteNoise 'α' AR
>     ;b = localBuf 'β' 2048 1
>     ;f = fft' b n
>     ;c = pv_BrickWall f (sinOsc KR 0.1 0 * 0.75)}
> in audition (out 0 (ifft' c * 0.1))

> import Sound.SC3.UGen.Protect

Variant with two local buffers

> let {n = uclone 'α' 2 (whiteNoise 'β' AR)
>     ;b = uclone 'γ' 2 (localBuf 'δ' 2048 1)
>     ;f = fft' b n
>     ;c = pv_BrickWall f (sinOsc KR (mce2 0.1 0.11) 0 * 0.75)}
> in audition (out 0 (ifft' c * 0.1))

Not clearing the buffer accesses old data, slowly overwrite data with noise

> let {b = localBuf 'α' 2048 2
>     ;nf = bufFrames KR b
>     ;x = mouseX KR 1 2 Linear 0.2
>     ;r = playBuf 2 AR b x 1 0 Loop DoNothing * 0.1
>     ;wr p i = bufWr b (linLin p (-1) 1 0 nf) Loop i
>     ;n = uclone 'β' 2 (whiteNoise 'γ' AR)
>     ;ph = lfNoise0 'δ' AR 530}
> in audition (mrg2 (out 0 r) (wr ph n))

bufCombC needs no clearing, because the delay line is filled by the ugen

> let {d = uclone 'α' 2 (dust 'β' AR 1)
>     ;n = whiteNoise 'γ' AR
>     ;z = decay d 0.3 * n
>     ;l = xLine KR 0.0001 0.01 20 DoNothing
>     ;sr = sampleRate
>     ;b = uclone 'δ' 2 (localBuf 'ε' sr 2)}
> in audition (out 0 (bufCombC b z l 0.2))

asLocalBuf combines localBuf and setBuf

> let {b = asLocalBuf 'α' [2,1,5,3,4,0]
>     ;x = mouseX KR 0 (bufFrames KR b) Linear 0.2
>     ;f = indexL b x * 100 + 40
>     ;o = saw AR (f * mce2 1 1.1) * 0.1}
> in audition (out 0 o)

detectIndex example using local buffer

> let {b = asLocalBuf 'α' [2,3,4,0,1,5]
>     ;n = bufFrames KR b
>     ;x = floorE (mouseX KR 0 n Linear 0.1)
>     ;i = detectIndex b x}
> in audition (out 0 (sinOsc AR (linExp i 0 n 200 700) 0 * 0.1))

degreeToKey example ('modal space') using local buffer

> let {n = lfNoise1 'α' KR (mce [3,3.05])
>     ;x = mouseX KR 0 15 Linear 0.1
>     ;b = asLocalBuf 'β' [0,2,3.2,5,7,9,10]
>     ;k = degreeToKey b x 12
>     ;mk_c bf = let {f0 = midiCPS (bf + k + n * 0.04)
>                    ;o = sinOsc AR f0 0 * 0.1
>                    ;f1 = midiCPS (mce [48,55])
>                    ;t = lfPulse AR f1 0.15 0.5
>                    ;f2 = midiCPS (sinOsc KR 0.1 0 * 10 + bf)
>                    ;d = rlpf t f2 0.1 * 0.1
>                    ;m = o + d}
>                 in combN m 0.31 0.31 2 + m}
> in audition (out 0 ((mk_c 48 + mk_c 72) * 0.25))
