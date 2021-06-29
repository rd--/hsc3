-- dbAmp ; linear db motion is exponential amplitude decay
fSinOsc ar 800 0 * dbAmp (line kr (-24) (-48) 10 RemoveSynth)

---- ; there is a non-UGen variant
dbAmp (-26::Double) == 0.05011872336272722
