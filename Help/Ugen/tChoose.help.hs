-- tChoose ; composite of tiRand and select
let x = mouseX kr 1 1000 Exponential 0.1
    t = dustId 'a' ar x
    f = midiCps (tiRandId 'b' 48 60 t)
    o = let a = mce [sinOsc ar f 0
                    ,saw ar (f * 2)
                    ,pulse ar (f * 0.5) 0.1]
        in tChooseId 'c' t a
in o * 0.1
