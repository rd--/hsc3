-- sampleDur ; the reciprocal of the nominal sample rate of the server
let f = mce2 sampleRate (recip sampleDur) * 0.01
in sinOsc ar f 0 * 0.1
