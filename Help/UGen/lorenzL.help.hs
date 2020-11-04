--lorenzL ; default parameters
lorenzL AR (sampleRate / 2) 10 28 2.667 0.05 0.1 0 0 * 0.2

-- lorenzL ; vary frequency
let x = mouseX KR 20 sampleRate Linear 0.1
in lorenzL AR x 10 27 2.667 0.05 0.1 0 0 * 0.3

-- lorenzL ; randomly modulate params
let n e = lfNoise0 e KR 0.5
    n0 = mul_add (n 'α') 2 10
    n1 = mul_add (n 'β') 20 38
    n2 = mul_add (n 'γ') 1.5 2
in lorenzL AR sampleRate n0 n1 n2 0.05 0.1 0 0 * 0.2

-- lorenzL ; frequency control
let x = mouseX KR 1 200 Linear 0.1
    n = lorenzL AR x 10 28 2.667 0.05 0.1 0 0
in sinOsc AR (lag n 0.003 * 800 + 900) 0 * 0.4

-- lorenzL
let x = mouseX KR 1 200 Linear 0.1
    n = lorenzL AR x 10 28 2.667 0.05 0.1 0 0
in impulse AR (n * 4 + 8) 0 * 0.4
