> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.HW.External as X {- hsc3 -}

Note: debian sc3-plugins doesn't build ladspalist, to build type:

    cd opt/src/sc3-plugins/source/LadspaUGen
    gcc -ldl search.c ladspalist.c -o ladspalist

CAPS = http://quitte.de/dsp/caps.html, http://packages.debian.org/stable/caps

    # 1767 C* ChorusI - Mono chorus/flanger
    > k: t (ms) (2.5 to 40)
    > k: width (ms) (0.5 to 10)
    > k: rate (Hz) (0.02 to 5)
    > k: blend (0 to 1)
    > k: feedforward (0 to 1)
    > k: feedback (0 to 1)
    > a: in (0 to 0)
    < a: out

> caps_1767_01 =
>   let s = soundIn 0
>       x = mouseX KR 0 1 Linear 0.2
>       y = mouseY KR 0 1 Linear 0.2
>       n1 = range 2.5 40 (lfNoise2 'α' KR 0.2)
>       n2 = range 0.5 10 (lfNoise2 'β' KR 0.2)
>   in X.ladspa 1 AR 1767 [n1,n2,0.5,0.5,x,y,s]

    # 1769 C* Click - Metronome
    > k: model (0 to 3)
    > k: bpm (4 to 240)
    > k: volume (0 to 1)
    > k: damping (0 to 1)
    < a: out

> caps_1769_01 =
>   let x = roundE (mouseX KR 0 3 Linear 0.2)
>       y = mouseY KR 4 240 Linear 0.2
>   in X.ladspa 1 AR 1769 [x,y,0.5,0.5]

    # 1773 C* Eq10 - 10-band equaliser
    > k: 31 Hz (-48 to 24)
    > k: 63 Hz (-48 to 24)
    > k: 125 Hz (-48 to 24)
    > k: 250 Hz (-48 to 24)
    > k: 500 Hz (-48 to 24)
    > k: 1 kHz (-48 to 24)
    > k: 2 kHz (-48 to 24)
    > k: 4 kHz (-48 to 24)
    > k: 8 kHz (-48 to 24)
    > k: 16 kHz (-48 to 24)
    > a: in (0 to 0)
    < a: out

> enumN n e = take n (enumFrom e)

> caps_1773_01 =
>   let s = soundIn 0
>       n = map (\z -> range (-24) 48 (lfNoise2 z KR 0.2)) (enumN 10 'α')
>   in X.ladspa 1 AR 1773 (n ++ [s]) * 0.5

    # 1771 C* Saturate - Various static nonlinearities, 8x oversampled
    > k: mode (0 to 11)
    > k: gain (dB) (-24 to 72)
    > k: bias (0 to 1)
    > a: in (0 to 0)
    < a: out

> caps_1771_01 =
>   let s = soundIn 0
>   in X.ladspa 1 AR 1771 [1,0,0,s]

> caps_1771_02 =
>   let s = soundIn 0
>       x = roundE (mouseX KR 0 11 Linear 0.2)
>       y = mouseY KR (-24) 72 Linear 0.2
>   in X.ladspa 1 AR 1771 [x,y,0.0,s]

    # 1772 C* Compress - Compressor and saturating limiter
    > k: measure (0 to 1)
    > k: mode (0 to 2)
    > k: threshold (0 to 1)
    > k: strength (0 to 1)
    > k: attack (0 to 1)
    > k: release (0 to 1)
    > k: gain (dB) (-12 to 18)
    < k: state (dB)
    > a: in (-1 to 1)
    < a: out

> caps_1772_01 =
>   let s = soundIn 0
>       x = roundE (mouseX KR 0 2 Linear 0.2)
>       y = mouseY KR 0 1 Linear 0.2
>   in X.ladspa 1 AR 1772 [0.5,x,y,0.5,0.5,0.1,0,s]

    # 1779 C* Plate - Versatile plate reverb
    > k: bandwidth (0 to 1)
    > k: tail (0 to 1)
    > k: damping (0 to 1)
    > k: blend (0 to 1)
    > a: in (0 to 0)
    < a: out.l
    < a: out.r

> caps_1779_01 =
>   let s = soundIn 0
>       x = mouseX KR 0 1 Linear 0.2
>       y = mouseY KR 0 1 Linear 0.2
>   in X.ladspa 2 AR 1779 [x,y,0.5,0.5,s]

    # 1788 C* Wider - Stereo image synthesis
    > k: pan (-1 to 1)
    > k: width (0 to 1)
    > a: in (0 to 0)
    < a: out.l
    < a: out.r

> caps_1788_01 =
>   let s = soundIn 0
>       x = mouseX KR (-1) 1 Linear 0.2
>       y = mouseY KR 0 1 Linear 0.2
>   in X.ladspa 2 AR 1788 [x,y,s]

    # 2586 C* PhaserII - Mono phaser
    > k: rate (0 to 1)
    > k: lfo (0 to 1)
    > k: depth (0 to 1)
    > k: spread (0 to 1)
    > k: resonance (0 to 1)
    > a: in (0 to 0)
    < a: out

> caps_2586_01 =
>   let s = soundIn 0
>       x = mouseX KR 0 1 Linear 0.2
>       y = mouseY KR 0 1 Linear 0.2
>   in X.ladspa 1 AR 2586 [0.3,x,0.5,y,0.8,s]

    # 2588 C* Scape - Stereo delay with chromatic resonances
    > k: bpm (30 to 164)
    > k: divider (2 to 4)
    > k: feedback (0 to 1)
    > k: dry (0 to 1)
    > k: blend (0 to 1)
    > k: tune (Hz) (415 to 467)
    > a: in (0 to 0)
    < a: out.l
    < a: out.r

> caps_2588_01 =
>   let s = soundIn 0
>       x = mouseX KR 30 164 Linear 0.2
>       y = roundE (mouseY KR 2 4 Linear 0.2)
>       n1 = lfNoise2 'α' KR 0.2
>       n2 = lfNoise2 'β' KR 0.2
>   in X.ladspa 2 AR 2588 [x,y,n1,n2,0.5,440,s]

    # 2592 C* AmpVTS - Idealised guitar amplification
    > k: over (0 to 2)
    > k: gain (0 to 1)
    > k: bright (0 to 1)
    > k: power (0 to 1)
    > k: tonestack (0 to 8)
    > k: bass (0 to 1)
    > k: mid (0 to 1)
    > k: treble (0 to 1)
    > k: attack (0 to 1)
    > k: squash (0 to 1)
    > k: lowcut (0 to 1)
    > a: in (0 to 0)
    < a: out

> caps_2592_def s = [1,0.25,0.75,0.5,1,0.25,1,0.75,0.75,0.25,0.5,s]

> caps_2592_01 = X.ladspa 1 AR 2592 (caps_2592_def (soundIn 0))

> caps_2592_02 =
>   let s = soundIn 0
>       x = roundE (mouseX KR 0 8 Linear 0.2)
>       y = mouseY KR 0 1 Linear 0.2
>       [n1,n2,n3,n4,n5,n6,n7,n8] = map (\z -> lfNoise2 z KR 0.2) (enumN 8 'α')
>   in X.ladspa 1 AR 2592 [1,y,n1,n2,x,n3,n4,n5,n6,n7,n8,s]

    # 2603 C* Spice - Not an exciter
    > k: lo.f (Hz) (50 to 400)
    > k: lo.compress (0 to 1)
    > k: lo.gain (0 to 1)
    > k: hi.f (Hz) (400 to 5000)
    > k: hi.gain (0 to 1)
    > a: in (0 to 0)
    < a: out

> caps_2603_01 =
>   let s = soundIn 0
>       x = mouseX KR 50 400 Exponential 0.2
>       y = mouseY KR 400 5000 Exponential 0.2
>   in X.ladspa 1 AR 2603 [x,0.5,0.5,y,0.5,s]

    # 2609 C* EqFA4p - 4-band parametric eq
    > k: a.act (0 to 1)
    > k: a.f (Hz) (20 to 14000)
    > k: a.bw (0.125 to 8)
    > k: a.gain (dB) (-24 to 24)
    > k: b.act (0 to 1)
    > k: b.f (Hz) (20 to 14000)
    > k: b.bw (0.125 to 8)
    > k: b.gain (dB) (-24 to 24)
    > k: c.act (0 to 1)
    > k: c.f (Hz) (20 to 14000)
    > k: c.bw (0.125 to 8)
    > k: c.gain (dB) (-24 to 24)
    > k: d.act (0 to 1)
    > k: d.f (Hz) (20 to 14000)
    > k: d.bw (0.125 to 8)
    > k: d.gain (dB) (-24 to 24)
    > k: gain (-24 to 24)
    < k: _latency
    > a: in (0 to 0)
    < a: out

> caps_2609_01 =
>   let s = soundIn 0
>       f z m l r = m (lfNoise2 z KR 0.2) (-1) 1 l r
>       p = [f 'α' linLin 0 1
>           ,f 'β' linExp 20 14000
>           ,f 'γ' linExp 0.125 8
>           ,f 'δ' linLin (-24) 24
>           ,f 'ε' linLin 0 1
>           ,f 'ζ' linExp 20 14000
>           ,f 'η' linExp 0.125 8
>           ,f 'θ' linLin (-24) 24
>           ,f 'ι' linLin 0 1
>           ,f 'κ' linExp 20 14000
>           ,f 'λ' linExp 0.125 8
>           ,f 'μ' linLin (-24) 24
>           ,f 'ν' linLin 0 1
>           ,f 'ξ' linExp 20 14000
>           ,f 'ο' linExp 0.125 8
>           ,f 'π' linLin (-24) 24
>           ,0
>           ,s]
>   in X.ladspa 1 AR 2609 p * 0.5
