> import Sound.SC3 {- hsc3 -}

Fixed frequency (hz) and initial-phase (radians)

> g_00 = sinOsc AR (midiCPS 69) 0 * 0.15 -- 415 440

Control input for frequency

> g_01 = sinOsc AR (midiCPS (control KR "mnn" 69)) 0 * 0.25

    import Sound.OSC {- hosc -}
    withSC3 (sendMessage (n_set1 (-1) "mnn" 64))

Modulate freq

> g_02 = sinOsc AR (xLine KR 2000 200 1 RemoveSynth) 0 * 0.5

Modulate freq

> g_03 =
>     let f1 = xLine KR 1 1000 9 RemoveSynth
>         f2 = sinOsc AR f1 0 * 200 + 800 -- (-1,1) ; (-200,200) ; (600,1000)
>     in sinOsc AR f2 0 * 0.25

Modulate phase

> g_04 =
>     let ph = sinOsc AR (xLine KR 20 8000 10 RemoveSynth) 0 * 2 * pi
>     in sinOsc AR 800 ph * 0.1

Mouse control

> g_05 =
>     let x = mouseX KR 40 10000 Exponential 0.2
>         y = mouseY KR 0.01 0.25 Exponential 0.2
>     in sinOsc AR x 0 * y

Simple bell-like tone.

> g_06 =
>     let f = mce [0.5,1,1.19,1.56,2,2.51,2.66,3.01,4.1]
>         a = mce [0.25,1,0.8,0.5,0.9,0.4,0.3,0.6,0.1]
>         o = sinOsc AR (500 * f) 0 * a
>         e = envGen KR 1 0.1 0 1 RemoveSynth (envPerc 0.01 10)
>     in mix o * e

"When two pure tones of slightly different frequency are superposed,
our ears perceive audible beats at a rate given by the difference of
the two frequencies."

> g_07 =
>     let f0 = 220
>         f1 = 221.25
>         d = abs (f1 - f0)
>         i = impulse AR d 0 * max (sinOsc KR 0.05 0 * 0.1) 0
>     in sinOsc AR (mce2 f0 f1) 0 * 0.1 + i

"When two tones are sounded together, a tone of lower frequency is
frequently heard. Such a tone is called a combination tone.  The most
commonly heard combination tone occurs at a frequency f2 - f1."

> g_08 =
>     let f1 = 300
>         f2 = 300 * 3/2 {- 450 -}
>         f3 = abs (f2 - f1) {- 150 -}
>         a3 = max (sinOsc KR 0.05 0 * 0.1) 0
>     in mix (sinOsc AR (mce3 f1 f2 f3) 0 * mce3 0.1 0.1 a3)

With frequency of zero, operates as table lookup variant of sin.

> mk_phasor (l,r) f = phasor AR 0 ((r - l) * f / sampleRate) l r l

> g_09 =
>   let ph = mk_phasor (0,two_pi) 440
>       o1 = sinOsc AR 440 0
>       o2 = sinOsc AR 0 ph
>       o3 = sin ph
>   in mce2 o1 (xFade2 o2 o3 (lfTri KR 0.1 0) 1) * 0.1

sync, ie. <https://www.listarc.bham.ac.uk/lists/sc-dev/msg58316.html>

> g_10 =
>   let dt = mce2 1.0 1.003
>	freq = mouseX KR (100 * dt) (3000 * dt) Exponential 0.2
>       sync_freq = mouseY KR 100 500 Exponential 0.2
>       ph_freq = impulse AR sync_freq 0 + impulse AR freq 0
>       o = sinOsc AR 0 (phasor AR ph_freq (freq / sampleRate) 0 1 0 * 2 * pi)
>   in o * 0.15

reverse sync

> g_11 =
>   let dt = mce2 1.0 1.003
>       freq = mouseX KR (100 * dt) (3000 * dt) Exponential 0.2
>       sync_freq = mouseY KR 100 500 Exponential 0.2
>       direction = toggleFF (impulse AR sync_freq 0) * (-2) + 1
>       o = sinOsc AR 0 (wrap (sweep (impulse AR freq 0) (direction * freq)) 0 (2 * pi))
>   in o * 0.15

reverse cycle & reverse sync

> g_12 =
>   let freq = mouseX KR (100 * mce2 1.0 1.003) (3000 * mce2 1.0 1.003) Exponential 0.2
>       sync_freq = mouseY KR 100 500 Exponential 0.2
>       direction = toggleFF (impulse AR sync_freq 0 + impulse AR freq 0) * (-2) + 1
>       o = sinOsc AR 0 (wrap (sweep (k2a 0) (direction * freq)) 0 (2 * pi))
>   in o * 0.5
