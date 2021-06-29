-- pauseSelf
let x = mouseX kr (-1) 1 Linear 0.1
    o = sinOsc ar 440 0 * 0.1
in mrg [o,pauseSelf x]

---- ; run paused node (assuming no intermediate node is created)
withSC3 (Sound.OSC.sendMessage (n_run [(-1, True)]))
