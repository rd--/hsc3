-- radiansPerSample ; two pi divided by the nominal sample rate (ie. a very small number)
let f = mce2 radiansPerSample ((2 * pi) / sampleRate) * 5000000
in sinOsc ar f 0 * 0.1
