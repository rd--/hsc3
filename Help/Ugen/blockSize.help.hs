-- blockSize ; default block size is 64 samples
sinOsc ar (mce2 (blockSize * 3) (64 * 3 + 1)) 0 * 0.1

-- blockSize
sinOsc ar (mce2 (blockSize * 3) ((controlDur * sampleRate * 3) + 1)) 0 * 0.1
