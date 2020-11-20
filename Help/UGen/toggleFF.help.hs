-- toggleFF ; c.f. setResetFF
let tr = dust 'α' AR (xLine KR 1 1000 60 DoNothing)
    fr = toggleFF tr * 400 + 800
in sinOsc AR fr 0 * 0.1
