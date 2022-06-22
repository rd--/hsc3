-- bufSampleRate ; requires=buf ; frequency as fraction of buffer sample-rate (ie. 48000 / 100 == 480)
let b = control kr "buf" 0
    f = mce [bufSampleRate kr b * 0.01, 440]
in sinOsc ar f 0 * 0.1

---- ; buffer setup
withSc3 (async (b_allocRead 0 (sfResolve "pf-c5.aif") 0 0))
