-- landon rose (jmcc) #8
let nt =
      [[32,43,54, 89]
      ,[10,34,80,120]
      ,[67,88,90,100]
      ,[76,88,99,124]]
    fr = map (map midi_to_cps) nt
    nd z e f =
      let p = klankSpec f (replicate 4 1) (replicate 4 3)
          x = e * pinkNoise z AR * mce2 0.0011 0.0012
      in klank x 1 0 1 p
    env i = abs (sinOsc AR (1 / 8) ((constant i / 2) * pi))
in sum (zipWith3 nd "αβγδ" (map env [0::Int .. 3]) fr)
