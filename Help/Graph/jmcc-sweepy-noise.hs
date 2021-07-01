-- sweepy noise (jmcc) #6
let n = mce2 (whiteNoise ar) (whiteNoise ar)
    lfoDepth = mouseY kr 200 8000 Exponential 0.1
    lfoRate = mouseX kr 4 60 Exponential 0.1
    freq = lfSaw kr lfoRate 0 * lfoDepth + (lfoDepth * 1.2)
    filtered = rlpf (n * 0.03) freq 0.1
in combN filtered 0.3 0.3 2 + filtered

-- sweepy noise (jmcc) #6 ; id
let n = mce2 (whiteNoiseId 'α' ar) (whiteNoiseId 'β' ar)
    lfoDepth = mouseY kr 200 8000 Exponential 0.1
    lfoRate = mouseX kr 4 60 Exponential 0.1
    freq = lfSaw kr lfoRate 0 * lfoDepth + (lfoDepth * 1.2)
    filtered = rlpf (n * 0.03) freq 0.1
in combN filtered 0.3 0.3 2 + filtered
