-- choose ; a composite of iRand and select
let f = Protect.uclone_all 'α' 2 (choose 'β' (mce [440,460 .. 880]))
in sinOsc AR f  0 * 0.1
