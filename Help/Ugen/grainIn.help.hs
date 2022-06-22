-- grainIn ; warning=feedback
let x = mouseX kr (-0.5) 0.5 Linear 0.1
    y = mouseY kr 5 25 Linear 0.1
    t = impulse kr y 0
in grainIn 2 t 0.1 (soundIn 0 + pinkNoiseId 'α' ar * 0.05) x (-1) 512 * 0.5
