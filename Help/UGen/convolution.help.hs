-- convolution ; warning=feedback
let k = pinkNoise ar * 0.1
    i = soundIn 0
in convolution i k 2048

-- convolution ; warning=feedback
let k = mix (lfSaw ar (mce [300,500,800,1000] * mouseX kr 1.0 2.0 Linear 0.2) 0 * 0.1)
    i = soundIn 0
in convolution i k 1024 * 0.5

-- convolution ; warning=feedback ; id
let k = pinkNoiseId 'α' ar * 0.1
    i = soundIn 0
in convolution i k 2048
