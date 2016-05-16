> Sound.SC3.UGen.Help.viewSC3Help "EnvGate"

> import Sound.SC3

Make envGate, giving the /default/ arguments, as used by envGate'.

> let {k = control KR
>     ;e = envGate 1 (k "gate" 1) (k "fadeTime" 0.02) RemoveSynth EnvSin}
> in audition (out 0 (lpf (saw AR 200) 600 * 0.1 * e))

Set fade time, then release gate.

> withSC3 (send (n_set1 (-1) "fadeTime" 2))
> withSC3 (send (n_set1 (-1) "gate" 0))

The same, but built in defaults.

> let e = envGate'
> in audition (out 0 (lpf (saw AR 200) 600 * 0.1 * e))

Several envGate nodes can coexist in one synth, but if they are the
same they're shared (as ever).

> let {e = envGate'
>     ;s1 = lpf (saw AR 80) 600 * e
>     ;s2 = rlpf (saw AR 200 * 0.5) (6000 * e + 60) 0.1 * e}
> in audition (out 0 (mce2 s1 s2 * 0.1))

> withSC3 (send (n_set1 (-1) "fadeTime" 5))
> withSC3 (send (n_set1 (-1) "gate" 0))
