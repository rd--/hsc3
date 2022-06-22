-- rdx7Env
let gate_ = control kr "gate" 0
    data_ = control kr "data" 0
    [r1,r2,r3,r4] = map (\k -> control kr ('r':show k) 99) [1 .. 4]
    [l1,l2,l3,l4] = map (\(k,v) -> control kr ('l':show k) v) (zip [1 .. 4] [99,99,99,00])
    ol = control kr "ol" 99
in sinOsc ar 440 0 * X.rdx7Env ar gate_ data_ r1 r2 r3 r4 l1 l2 l3 l4 ol

---- ; env set
withSc3 (Sound.OSC.sendMessage (n_set (-1) [("r1",35.0),("r2",65),("r3",55),("r4",25)]))
withSc3 (Sound.OSC.sendMessage (n_set (-1) [("l1",99.0),("l2",65),("l3",75),("l4",0)]))
withSc3 (Sound.OSC.sendMessage (n_set1 (-1) "gate" 1.0))
withSc3 (Sound.OSC.sendMessage (n_set1 (-1) "gate" 0.0))
