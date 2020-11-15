-- sendReply
let s0 = lfNoise0 'α' KR 5
    s1 = lfNoise0 'β' KR 5
    o = sinOsc AR (s0 * 200 + 500) 0 * s1 * 0.1
in mrg [o,sendReply s0 0 "/send-reply" [s0,s1]]

---- ; receive reply
withSC3 (withNotifications (Sound.OSC.waitReply "/send-reply"))