-- meddis ; passing a signal through the hair cell
X.meddis (sinOsc ar 440 0)

-- meddis ; recursive hair cell compression
X.meddis (X.meddis (soundIn 0))
