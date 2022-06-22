-- mce ; nested ; channel layout is L=440,441 and R=660,661
let f = mce2 (mce2 440 660) (mce2 441 661)
in mix (sinOsc ar f 0 * 0.1)

-- mceFill ; c.f. uclone_all
mceFillInt 2 (\z -> brownNoiseId z ar * 0.05)

-- mceFillId
mceFillId 'α' 2 (\z i -> brownNoiseId z ar * 0.025 * (i + 1))

-- mce ; one out ; two channels
sinOsc ar (mce2 440 441) 0 * 0.1

-- mce ; two out ; each single channel ; hence mono
sinOsc ar (mce1 (mce2 440 441)) 0 * 0.1

---- mce ; isList ; :set -XOverloadedLists (~/.ghci)
sinOsc ar [440,441] 0 * 0.1

---- mce ; isList ; :set -XOverloadedLists (~/.ghci) ; channel layout is L=440,441 and R=660,661
mix (sinOsc ar [[440,660],[441,661]] 0) * 0.1

---- ; c.f uclone_all
Protect.uclone_allId 'α' 2 (brownNoiseId 'β' ar) * 0.05
