-- choose ; a composite of iRand and select
let f = mceFillId 'α' 2 (\z _ -> chooseId z (mce [440,460 .. 880]))
in sinOsc ar f  0 * 0.1
