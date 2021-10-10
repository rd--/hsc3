-- sndfileIn ; diskIn with "brackets" to allocate and read and then close and free the sndfile buffer
sinOsc ar 440 0 * 0.05 + sndfileIn (0,"buf") "20.2-LW+RD.flac" Loop

-- sndfileIn ; requires=buf ; a sndfileIn graph is equal to the below, excepting that here the number of channels is written out
sinOsc ar 440 0 * 0.05 + diskIn 2 (control kr "buf" 0) Loop

---- ; print scsynth, which holds the reference thats stores end brackets
scsynthPrint scsynth
