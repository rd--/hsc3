-- choose ; a composite of iRand and select
let f = mceFill_z 'α' 2 (\z _ -> choose z (mce [440,460 .. 880]))
in sinOsc AR f  0 * 0.1
