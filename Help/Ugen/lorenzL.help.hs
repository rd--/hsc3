--lorenzL ; default parameters
lorenzL ar (sampleRate / 2) 10 28 2.667 0.05 0.1 0 0 * 0.2

-- lorenzL ; vary frequency
let x = mouseX kr 20 sampleRate Linear 0.1
in lorenzL ar x 10 27 2.667 0.05 0.1 0 0 * 0.3

-- lorenzL ; randomly modulate params
let nId e = lfNoise0Id e kr 0.5
    n0 = mul_add (nId 'α') 2 10
    n1 = mul_add (nId 'β') 20 38
    n2 = mul_add (nId 'γ') 1.5 2
in lorenzL ar sampleRate n0 n1 n2 0.05 0.1 0 0 * 0.2

-- lorenzL ; frequency control
let x = mouseX kr 1 200 Linear 0.1
    n = lorenzL ar x 10 28 2.667 0.05 0.1 0 0
in sinOsc ar (lag n 0.003 * 800 + 900) 0 * 0.4

-- lorenzL
let x = mouseX kr 1 200 Linear 0.1
    n = lorenzL ar x 10 28 2.667 0.05 0.1 0 0
in impulse ar (n * 4 + 8) 0 * 0.4
