-- sndfileDiskIn ; either diskIn or vDiskIn with "brackets" to allocate and read and then close and free the sndfile buffer
sinOsc ar 440 0 * 0.05 + sndfileDiskIn (0, "dsk", [0, 1]) "20.2-LW+RD.flac" Nothing Loop

-- sndfileIn ; diskIn form of sndfileDiskIn
sinOsc ar 440 0 * 0.05 + sndfileIn (0, "dsk", []) "20.2-LW+RD.flac" Loop

-- sndfileVarIn ; vDiskIn with "brackets" ; if readChan is empty all channels are read
sinOsc ar 440 0 * 0.05 + sndfileVarIn (0, "dsk", []) "20.2-LW+RD.flac" (sinOsc kr 0.1 0 * 0.015 + 1) Loop

-- sndfileIn ; requires=buf ; a sndfileIn graph is equal to the below, excepting that here the number of channels is written out
sinOsc ar 440 0 * 0.05 + diskIn 2 (control kr "dsk" 0) Loop

---- ; print scsynth, the interpreter value that holds the reference that stores the end brackets
scsynthPrint scsynth
