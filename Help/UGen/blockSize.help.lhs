> import Sound.SC3 {- hsc3 -}

default block size is 64 samples

> g_01 = sinOsc AR (mce2 (blockSize * 3) (64 * 3 + 1)) 0 * 0.1

> g_02 = sinOsc AR (mce2 (blockSize * 3) ((controlDur * sampleRate * 3) + 1)) 0 * 0.1
