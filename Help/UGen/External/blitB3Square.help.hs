-- Sound.SC3.UGen.Help.viewSC3Help "BlitB3Square"
-- Sound.SC3.UGen.DB.ugenSummary "BlitB3Square"

import Sound.SC3.ID

-- > audition (out 0 (sq1 * 0.1))
sq1 =
    let f = xLine KR 1000 20 10 DoNothing
    in blitB3Square AR f 0.99

-- aliasing suddenly appears for very high frequencies
--
-- > audition (out 0 (sq2 * 0.1))
sq2 =
    let f = mouseX KR 20 10000 Exponential 0.2
        c = mouseY KR 0.001 0.999 Linear 0.2
    in blitB3Square AR f c

-- difference in CPU usage (excessive wire use,-w 1024)
--
-- > audition (out 0 (sq3 (\rt f -> blitB3Square rt f 0.99)))
-- > audition (out 0 (sq3 (\rt f -> pulse rt f 0.5)))
sq3 sqr_osc =
    let f z = midiCPS (range 36 72 (lfNoise0 z KR (rand z 2 3)))
        l z = rand z (-1) 1
        o z = pan2 (sqr_osc AR (f z) * 0.1) (l z) 0.1
    in sum (map o [0::Int .. 99])
