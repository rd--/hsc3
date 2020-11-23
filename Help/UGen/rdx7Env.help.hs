-- rdx7Env
let gate_ = control KR "gate" 0
    data_ = control KR "data" 0
    [r1,r2,r3,r4] = map (\k -> control KR ('r':show k) 99) [1 .. 4]
    [l1,l2,l3,l4] = map (\(k,v) -> control KR ('l':show k) v) (zip [1 .. 4] [99,99,99,00])
    ol = control KR "ol" 99
in sinOsc AR 440 0 * X.rdx7Env AR gate_ data_ r1 r2 r3 r4 l1 l2 l3 l4 ol

withSC3 (Sound.OSC.sendMessage (n_set (-1) [("r1",35.0),("r2",65),("r3",55),("r4",25)]))
withSC3 (Sound.OSC.sendMessage (n_set (-1) [("l1",99.0),("l2",65),("l3",75),("l4",0)]))
withSC3 (Sound.OSC.sendMessage (n_set1 (-1) "gate" 1.0))
withSC3 (Sound.OSC.sendMessage (n_set1 (-1) "gate" 0.0))
