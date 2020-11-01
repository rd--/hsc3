-- sms ; sine reconstruction left channel, noises on right (sample rate must be 44100)
let z = soundIn 0
    y = mouseY KR 1 50 Linear 0.2
    x = mouseX KR 0.5 4 Linear 0.2
in X.sms {- AR -} z 50 y 8 0.3 x 0 0 0 1 (-1)

-- sms ; default param
let z = soundIn 0
in X.sms {- AR -} z 80 80 4 0.2 1 0 0 0 1 (-1)
