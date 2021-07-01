-- choose ; a composite of iRand and select
let f = mceFill_z 'α' 2 (\z _ -> chooseId z (mce [440,460 .. 880]))
in sinOsc ar f  0 * 0.1
