-- controlDur ; default block size = 64, default sample rate = 48000
sinOsc ar (mce2 (recip controlDur) (controlRate + 1)) 0 * 0.1

-- controlDur
sinOsc ar (mce2 (recip controlDur) (recip (blockSize / sampleRate) + 1)) 0 * 0.1
