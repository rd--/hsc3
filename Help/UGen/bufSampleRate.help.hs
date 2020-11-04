-- bufSampleRate ; requires=buf ; frequency as fraction of buffer sample-rate (ie. 48000 / 100 == 480)
let b = control KR "buf" 0
    f = mce [bufSampleRate KR b * 0.01, 440]
in sinOsc AR f 0 * 0.1
