-- http://www.create.ucsb.edu/pipermail/sc-users/2007-August/035957.html
let time = 24
    steam =
      let n1 = whiteNoise 'α' AR
          n2 = pinkNoise 'β' AR
          piston = lfSaw AR (xLine AR 1 7 time DoNothing) 0
          air = lpf (n1 * piston + n2 * piston) 5000
          e = envGen AR 1 1 0 1 DoNothing (envSine time 9)
      in bpf air 600 (1 + e)
    whistle =
      let n3 = whiteNoise 'γ' AR
          f = [800,600,1200,990]
          s = klankSpec f [1,1,1,1] [1,1,1,1]
          t = [0,0,1,1,0,0,1,1,0,0,1,0]
          l = [2,0,0.2,0,0.2,0,0.8,0,4,0,3]
          d = Envelope t l (repeat EnvLin) Nothing Nothing 0
          e = envGen AR 1 1 0 (time / 10) DoNothing d
      in klank (n3 * 0.004) 1 0 1 s * e
    loc = let e = Envelope [-0.8,0.8] [time + 2] [EnvSin,EnvSin] Nothing Nothing 0
          in envGen AR 1 1 0 1 RemoveSynth e
in pan2 (steam + whistle) loc 1
