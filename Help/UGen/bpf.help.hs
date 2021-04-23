-- bpf
let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in bpf (saw AR 200 * 0.5) f 0.3

-- bpf
let x = mouseX KR 100 10000 Exponential 0.2
in bpf (saw AR 200 * 0.5) x 0.3

-- bpf
let n = whiteNoise 'α' AR
    x = mouseX KR 220 440 Exponential 0.1
    y = mouseY KR 0.01 0.2 Linear 0.1
in bpf n (mce [x, 550 - x]) y

-- bpf ; of control signals
let vib = bpf (pinkNoise 'α' KR) (mouseX KR 1 100 Exponential 0.2) 0.3 * 10
in sinOsc AR (vib * 200 + 600) 0 * 0.1

-- bpf ; ln 2021-04-11 https://lukasnowok.github.io/spectrology/
let n = bpf (whiteNoise 'α' AR) 10000 (xLine AR 2 0.002 20 DoNothing)
    o = sinOsc AR 10000 0 * xLine AR 0.001 0.3 20 DoNothing
in (n + o) * 0.1

---- ; drawings
UI.ui_baudline 4096 50 "linear" 2
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.05 (bpf (whiteNoise 'α' AR) 440 0.01)
