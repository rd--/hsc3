-- delayN ; dust randomly triggers decay to envelope WhiteNoise ; input is left, delay right
let i = decay (dust 'α' ar 1) 0.3 * whiteNoise 'β' ar * 0.1
in mce2 i (delayN i 0.1 0.1)

-- delayN ; delay time varied at control rate ; sinOsc reinforcing or cancelling with delayed copy
let i = sinOsc ar 320 0 * 0.1
    maxdelaytime = 0.005
    delaytime = mouseX kr 0.0 maxdelaytime Linear 0.15
in i + delayN i maxdelaytime delaytime

-- delayN ; flanging ; warning=feedback
let f = 0.1 -- flanger freq
    g = 0.1 -- feedback
    i = soundIn (mce2 0 1) -- two channels of input signal
    fb = i + localIn 2 ar 0 -- add feedback
    e = delayN fb 0.02 (sinOsc kr f 0 * 0.005 + 0.005) -- max delay of 20msec
    lp_f x = bpf x (mouseX kr 1000 10000 Linear 0.2) 0.1 -- filter in the feedback loop
    o = localOut (lp_f e * g)
in mrg2 (i + e) o
