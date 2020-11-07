> import Sound.SC3 {- hsc3 -}

The Q factor of a resonator is defined as the center-frequency (cf) divided by the bandwidth (bw).

    > let q cf bw = cf / bw

The higher the Q the narrower the filter.

    > let bw_set = [1,4,10,100,200,400]
    > map (q 400) bw_set == [400,100,40,4,2,1]

Q multipled by the band-width gives the center-frequency.

    > map (\bw -> q 400 bw * bw) bw_set == replicate (length bw_set) 400

The third argument to resonz is the reciprocal of Q (rq).

    > let rq cf = recip . q cf
    > map (rq 400) [1,4,10,100,200,400] == [1/400,1/100,1/40,1/4,1/2,1]

1/Q multiplied by the center-frequency gives the bandwidth.

    > map (\bw -> rq 400 bw * 400) bw_set == bw_set

> g_01 =
>   let n = whiteNoise 'α' AR
>   in resonz (n * 0.5) 2000 0.1

Modulate frequency

> g_02 =
>   let n = whiteNoise 'α' AR
>       f = xLine KR 1000 8000 10 RemoveSynth
>   in resonz (n * 0.5) f 0.05

Modulate bandwidth

> g_03 =
>   let n = whiteNoise 'α' AR
>       bw = xLine KR 1 0.001 8 RemoveSynth
>   in resonz (n * 0.5) 2000 bw

Modulate bandwidth opposite direction

> g_04 =
>   let n = whiteNoise 'α' AR
>       bw = xLine KR 0.001 1 8 RemoveSynth
>   in resonz (n * 0.5) 2000 bw

Mouse exam (1/Q = bandwidth / center-frequency)

> g_05 =
>   let n = pinkNoise 'α' AR
>       m = mouseX KR 36 85 Linear 0.2 {- midi note -}
>       w = mouseY KR 0.1 5 Linear 0.2 {- bandwidth -}
>       f = midiCPS (floorE m) {- centre frequency -}
>       rq = w / f {- 1/Q (reciprocal of Q) -}
>   in resonz (n * 0.5) f rq
