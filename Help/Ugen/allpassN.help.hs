-- allpassN ; add input to the filtered sound to hear the effect of the phase comb
let n = whiteNoiseId 'β' ar * 0.05
    dly = xLine kr 0.0001 0.01 20 RemoveSynth
in n + allpassN n 0.01 dly 0.2

-- allpassN ; used as an echo, c.f. comb ; outputs signal immediately (inverted), lower amplitude
let n = whiteNoiseId 'ε' ar
    d = dustId 'ζ' ar 1
    src = decay (d * 0.25) 0.2 * n
in allpassN src 0.2 0.2 3

-- allpassN ; phasing ; warning=feedback
let i = soundIn (mce2 0 1) -- two channels of input signal
    f = mouseX kr 0.1 1.0 Linear 0.2 -- phaser freq
    e = allpassN i 0.02 (sinOsc kr f 0 * 0.01 + 0.01) 1 -- max delay of 20msec
in i + e -- sum phase-shifted signal to original signal
