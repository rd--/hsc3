-- Sound.SC3.UGen.Help.viewSC3Help "PV_MagMul"
-- Sound.SC3.UGen.DB.ugenSummary "PV_MagMul"

import Sound.SC3.ID

-- > withSC3 (do {_ <- async (b_alloc 0 2048 1)
-- >             ;async (b_alloc 1 2048 1)})

-- > audition (out 0 (mm0 (whiteNoise 'a' AR * 0.2)))
-- > audition (out 0 (mm0 (soundIn 4 * 0.5)))
mm0 z =
    let y = lfSaw AR (midiCPS 43) 0 * 0.2
        c0 = fft' (localBuf 'a' 2048 1) y
        c1 = fft' (localBuf 'b' 2048 1) z
        c2 = pv_MagMul c0 c1
    in ifft' c2 * 0.1
