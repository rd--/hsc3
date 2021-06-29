-- mce ; channel layout is L=440,441 and R=660,661
let f = mce2 (mce2 440 660) (mce2 441 661)
in mix (sinOsc ar f 0 * 0.1)

-- mceFill ; c.f. uclone_all
mceFillInt 2 (\z -> brownNoise z ar * 0.05)

-- mceFill_z
mceFill_z 'α' 2 (\z i -> brownNoise z ar * 0.025 * (i + 1))

---- ; c.f uclone_all
Protect.uclone_all 'α' 2 (brownNoise 'β' ar) * 0.05
