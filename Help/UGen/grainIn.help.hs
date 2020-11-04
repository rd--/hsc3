-- grainIn ; warning=feedback
let x = mouseX KR (-0.5) 0.5 Linear 0.1
    y = mouseY KR 5 25 Linear 0.1
    t = impulse KR y 0
in grainIn 2 t 0.1 (soundIn 0 + pinkNoise 'α' AR * 0.05) x (-1) 512 * 0.5
