> import Sound.SC3 {- hsc3 -}

default block size = 64, default sample rate = 48000

> g_01 = sinOsc AR (mce2 (recip controlDur) (controlRate + 1)) 0 * 0.1

> g_02 = sinOsc AR (mce2 (recip controlDur) (recip (blockSize / sampleRate) + 1)) 0 * 0.1
