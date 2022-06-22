-- ramp ; lag pitch
let o = lfPulse kr 4 0 0.5 * 50 + 400
    l = line kr 0 1 15 DoNothing
    f = ramp o l
in sinOsc ar f 0 * 0.3

-- ramp ; mouse control
let x1 = mouseX kr 220 440 Exponential 0
    x2 = ramp x1 (300 / 1000)
in sinOsc ar (mce2 x1 x2) 0 * 0.1

-- ramp ; control inputs
let db = control kr "db" (-60)
    dur = control kr "dur" 0.1
in sinOsc ar 110 0 * ramp (dbAmp db) dur

---- ; initiate ramps
withSC3 (Sound.OSC.sendMessage (n_set 1 [("db",-6),("dur",1)]))
withSC3 (Sound.OSC.sendMessage (n_set 1 [("db",-90),("dur",1)]))

