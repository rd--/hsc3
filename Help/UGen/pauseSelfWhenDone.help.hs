-- pauseSelfWhenDone
let x = mouseX KR (-1) 1 Linear 0.1
    e = linen x 1 0.1 1 DoNothing
    o = sinOsc AR 440 0 * e
in mrg [o,pauseSelfWhenDone e]

---- ; run paused node (assuming no intermediate node is created)
withSC3 (Sound.OSC.sendMessage (n_run [(-1, True)]))
