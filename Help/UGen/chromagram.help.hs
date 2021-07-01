-- chromagram
let sig = soundIn 0
    chn = fft' (localBufId 'α' 1 2048) sig
    chroma = X.chromagram kr chn 2048 12 (midiCPS 24) 8 0 0.9 2 0
in splay (sinOsc ar (mce (map midiCPS [60 .. 71])) 0 * lag chroma 0.1 * 0.2) 1 1 0 True
