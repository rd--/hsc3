-- sendReply
let s0 = lfNoise0Id 'α' kr 5
    s1 = lfNoise0Id 'β' kr 5
    o = sinOsc ar (s0 * 200 + 500) 0 * s1 * 0.1
in mrg [o,sendReply s0 0 "/send-reply" [s0,s1]]

---- ; receive reply
withSc3 (withNotifications (Sound.OSC.waitReply "/send-reply"))
