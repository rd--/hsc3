-- tScramble ; two channel sin tones, channels scramble on impulse, scramble may be identity
let tr = impulse kr 1 0
in sinOsc ar (X.tScrambleId 'α' tr (mce2 440 880)) 0 * 0.1

-- tScramble ; c-major sin tones across stereo field, scrambles on impulse
let tr = impulse kr 1 0
    f = X.tScrambleId 'α' tr (mce (map midiCps [60,62,64,65,67,69,71,72]))
in splay (sinOsc ar f 0 * 0.1) 1 1 0 True
