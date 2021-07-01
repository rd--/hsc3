-- ladspa ; caps ; # 1767 C* ChorusI - Mono chorus/flanger
let s = soundIn 0
    x = mouseX kr 0 1 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
    n1 = range 2.5 40 (lfNoise2Id 'α' kr 0.2)
    n2 = range 0.5 10 (lfNoise2Id 'β' kr 0.2)
in X.ladspa 1 ar 1767 [n1,n2,0.5,0.5,x,y,s]

-- ladspa ; caps ; # 1769 C* Click - Metronome
let x = roundE (mouseX kr 0 3 Linear 0.2)
    y = mouseY kr 4 240 Linear 0.2
in X.ladspa 1 ar 1769 [x,y,0.5,0.5]

-- ladspa ; caps ; # 1773 C* Eq10 - 10-band equaliser
let s = soundIn 0
    enumN n e = take n (enumFrom e)
    n = map (\z -> range (-24) 48 (lfNoise2Id z kr 0.2)) (enumN 10 'α')
in X.ladspa 1 ar 1773 (n ++ [s]) * 0.1

-- ladspa ; caps ; # 1771 C* Saturate - Various static nonlinearities, 8x oversampled
let s = soundIn 0
in X.ladspa 1 ar 1771 [1,0,0,s]

-- ladspa ; caps ; # 1771 C* Saturate - Various static nonlinearities, 8x oversampled
let s = soundIn 0
    x = roundE (mouseX kr 0 11 Linear 0.2)
    y = mouseY kr (-24) 72 Linear 0.2
in X.ladspa 1 ar 1771 [x,y,0.0,s]

-- ladspa ; caps ; # 1772 C* Compress - Compressor and saturating limiter
let s = soundIn 0
    x = roundE (mouseX kr 0 2 Linear 0.2)
    y = mouseY kr 0 1 Linear 0.2
in X.ladspa 1 ar 1772 [0.5,x,y,0.5,0.5,0.1,0,s]

-- ladspa ; caps ; # 1779 C* Plate - Versatile plate reverb
let s = soundIn 0
    x = mouseX kr 0 1 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
in X.ladspa 2 ar 1779 [x,y,0.5,0.5,s]

-- ladspa ; caps ; # 1788 C* Wider - Stereo image synthesis
let s = soundIn 0
    x = mouseX kr (-1) 1 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
in X.ladspa 2 ar 1788 [x,y,s]

-- ladspa ; caps ; # 2586 C* PhaserII - Mono phaser
let s = soundIn 0
    x = mouseX kr 0 1 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
in X.ladspa 1 ar 2586 [0.3,x,0.5,y,0.8,s]

-- ladspa ; caps ; # 2592 C* AmpVTS - Idealised guitar amplification
X.ladspa 1 ar 2592 [1,0.25,0.75,0.5,1,0.25,1,0.75,0.75,0.25,0.5,soundIn 0] * 0.1

-- ladspa ; caps ; # 2592 C* AmpVTS - Idealised guitar amplification
let s = soundIn 0
    x = roundE (mouseX kr 0 8 Linear 0.2)
    y = mouseY kr 0 1 Linear 0.2
    enumN n e = take n (enumFrom e)
    [n1,n2,n3,n4,n5,n6,n7,n8] = map (\z -> lfNoise2Id z kr 0.2) (enumN 8 'α')
in X.ladspa 1 ar 2592 [1,y,n1,n2,x,n3,n4,n5,n6,n7,n8,s]

-- ladspa ; caps ; # 2609 C* EqFA4p - 4-band parametric eq
let s = soundIn 0
    fId z m l r = m (lfNoise2Id z kr 0.2) (-1) 1 l r
    p = [fId 'α' linLin 0 1
        ,fId 'β' linExp 20 14000
        ,fId 'γ' linExp 0.125 8
        ,fId 'δ' linLin (-24) 24
        ,fId 'ε' linLin 0 1
        ,fId 'ζ' linExp 20 14000
        ,fId 'η' linExp 0.125 8
        ,fId 'θ' linLin (-24) 24
        ,fId 'ι' linLin 0 1
        ,fId 'κ' linExp 20 14000
        ,fId 'λ' linExp 0.125 8
        ,fId 'μ' linLin (-24) 24
        ,fId 'ν' linLin 0 1
        ,fId 'ξ' linExp 20 14000
        ,fId 'ο' linExp 0.125 8
        ,fId 'π' linLin (-24) 24
        ,0
        ,s]
in pan2 (X.ladspa 1 ar 2609 p) (fId 'ρ' linLin (-1) 1) 0.25

{---- ; Note: debian sc3-plugins doesn't build ladspalist, to build type:

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

    # 1769 C* Click - Metronome
    > k: model (0 to 3)
    > k: bpm (4 to 240)
    > k: volume (0 to 1)
    > k: damping (0 to 1)
    < a: out

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

    # 1771 C* Saturate - Various static nonlinearities, 8x oversampled
    > k: mode (0 to 11)
    > k: gain (dB) (-24 to 72)
    > k: bias (0 to 1)
    > a: in (0 to 0)
    < a: out

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

    # 1779 C* Plate - Versatile plate reverb
    > k: bandwidth (0 to 1)
    > k: tail (0 to 1)
    > k: damping (0 to 1)
    > k: blend (0 to 1)
    > a: in (0 to 0)
    < a: out.l
    < a: out.r

    # 1788 C* Wider - Stereo image synthesis
    > k: pan (-1 to 1)
    > k: width (0 to 1)
    > a: in (0 to 0)
    < a: out.l
    < a: out.r

    # 2586 C* PhaserII - Mono phaser
    > k: rate (0 to 1)
    > k: lfo (0 to 1)
    > k: depth (0 to 1)
    > k: spread (0 to 1)
    > k: resonance (0 to 1)
    > a: in (0 to 0)
    < a: out

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

    # 2603 C* Spice - Not an exciter
    > k: lo.f (Hz) (50 to 400)
    > k: lo.compress (0 to 1)
    > k: lo.gain (0 to 1)
    > k: hi.f (Hz) (400 to 5000)
    > k: hi.gain (0 to 1)
    > a: in (0 to 0)
    < a: out

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
-}

---- ladspa ; caps ; # 2588 C* Scape - Stereo delay with chromatic resonances
let s = soundIn 0 * 0.1
in X.ladspa 2 ar 2588 [30,2,0,0.1,0.5,415,s] * 0.1

---- ladspa ; caps ; # 2588 C* Scape - Stereo delay with chromatic resonances
let s = soundIn 0 * 0.1
    x = mouseX kr 30 164 Linear 0.2
    y = roundE (mouseY kr 2 4 Linear 0.2)
    n1 = lfNoise2Id 'α' kr 0.2 * 0.5 + 0.5
    n2 = lfNoise2Id 'β' kr 0.2 * 0.5 + 0.5
in X.ladspa 2 ar 2588 [x,y,n1,n2,0.5,440,s]

---- ladpsa ; caps ; # 2603 C* Spice - Not an exciter
let s = soundIn 0
    x = mouseX kr 50 400 Exponential 0.2
    y = mouseY kr 400 5000 Exponential 0.2
in X.ladspa 1 ar 2603 [x,0.5,0.5,y,0.5,s]
