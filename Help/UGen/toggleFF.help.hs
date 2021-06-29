-- toggleFF ; c.f. setResetFF
let tr = dust 'α' ar (xLine kr 1 1000 60 DoNothing)
    fr = toggleFF tr * 400 + 800
in sinOsc ar fr 0 * 0.1
