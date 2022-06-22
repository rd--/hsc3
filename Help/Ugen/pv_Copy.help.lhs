> import Sound.Sc3 {- hsc3 -}

Proof of concept, silence.  This graph is disallowed.  pv_Copy is
required to run before the ifft of c0, which mutates c0, however that
is not apparent from the edge structure of the graph.  See instead
PV_Split.

> g_01 =
>     let z = lfClipNoiseId 'α' ar 100 * 0.1
>         c0 = fft' (localBufId 'β' 2048 1) z
>         c1 = pv_Copy c0 (localBufId 'γ' 2048 1)
>     in ifft' c1 - ifft' c0

The equivalent situation in sclang.

{var z = LFClipNoise.ar(100) * 0.1
;var g = FFT(LocalBuf(2048),z)
;IFFT(g) - IFFT(PV_Copy(g,LocalBuf(2048)))}.play

{var z = LFClipNoise.ar(100) * 0.1
;var g = FFT(LocalBuf(2048),z)
;IFFT(PV_Copy(g,LocalBuf(2048))) - IFFT(g)}.play

{var z = LFClipNoise.ar(100) * 0.1
;var g = FFT(LocalBuf(2048),z)
;var h = PV_Copy(g,LocalBuf(2048))
;IFFT(g) - IFFT(h)}.play
