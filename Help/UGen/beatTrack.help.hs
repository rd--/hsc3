-- beatTrack
let c = localBuf 'α' 1 1024
    i = soundIn 0
    x = mouseX kr (-1) 1 Linear 0.2
    [b, h, q, t] = mceChannels (beatTrack kr (fft' c i) x)
    f = mce [440, 660, 880]
    a = mce [0.4, 0.2, 0.1]
    s = mix (sinOsc ar f 0 * a * decay (mce [b, h, q]) 0.05)
in i + s


