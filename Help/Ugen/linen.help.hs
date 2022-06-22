-- linen
let e = linen (impulse kr 2 0) 0.01 0.6 0.4 DoNothing
in e * sinOsc ar 440 0 * 0.1

-- linen ; mouseX is envelope trigger
let x = mouseX kr (-1) 1 Linear 0.1
    y = mouseY kr 0.01 0.25 Linear 0.1
    e = linen x 1 y 1.0 DoNothing
in sinOsc ar 440 0 * e

-- linen ; PauseSynth done action
let x = mouseX kr (-1) 1 Linear 0.1
    e = linen x 1 0.1 1 PauseSynth
in sinOsc ar 440 0 * e

---- ; run paused node (assuming no intermediate node is created)
withSc3 (Sound.OSC.sendMessage (n_run [(-1, True)]))
