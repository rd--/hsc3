-- rTScramble ; two channel sin tones, channels scramble on impulse, scramble may be identity
let tr = impulse KR 1 0
in sinOsc AR (X.rTScramble 'α' tr (mce2 440 880)) 0 * 0.1

-- rTScramble ; c-major sin tones across stereo field, scrambles on impulse
let tr = impulse KR 1 0
    f = X.rTScramble 'α' tr (mce (map midiCPS [60,62,64,65,67,69,71,72]))
in splay (sinOsc AR f 0 * 0.1) 1 1 0 True
