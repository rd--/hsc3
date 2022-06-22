-- shaper ; hear waveshaper at pure (sin) tone ; generate table at client and use localBuf
let z = sinOsc ar 300 0 * line kr 0 1 12 DoNothing
    c = Gen.cheby 256 [1, 0, 1, 1, 0, 1]
    t = to_wavetable_nowrap c
    b = asLocalBufId 'α' t
in shaper b z * 0.1

-- shaper ; hear waveshaper at pure (sin) tone ; local wavetable
let z = sinOsc ar 300 0 * line kr 0 1 6 DoNothing
    t = Gen.chebyShaperTbl 256 [1, 0, 1, 1, 0, 1]
in shaper (asLocalBuf t) z * 0.1

-- shaper ; minor variation
let z = sinOsc ar 400 (pi / 2) * line kr 0 1 6 DoNothing
in shaper (asLocalBuf (Gen.chebyShaperTbl 256 [1, 0, 1, 1, 0, 1])) z * 0.1

-- shaper ; mouse control
let z = sinOsc ar 400 (pi / 2) * mouseY kr 0.01 1 Exponential 0.2
    t = asLocalBuf (Gen.chebyShaperTbl 256 [1, 0, 1, 1, 0, 1])
in shaper t z * 0.1

-- shaper ; sound in ; mouse control ; warning=feedback
let z = soundIn 0
    x = mouseX kr (-1) 1 Linear 0.2
    t = asLocalBuf (Gen.chebyShaperTbl 256 [1, 0, 1, 1, 0, 1])
in xFade2 z (shaper t z) x 0.5

-- shaper ; hear waveshaper at pure (sin) tone ; requires=tbl see b_gen_cheby at b_gen help file
let z = sinOsc ar 300 0 * line kr 0 1 6 DoNothing
in shaper (control kr "tbl" 10) z * 0.1

-- shaper ; minor variation ; requires=tbl
let z = sinOsc ar 400 (pi / 2) * line kr 0 1 6 DoNothing
in shaper (control kr "tbl" 10) z * 0.1

-- shaper ; wave shape external signal ; requires=tbl
let z = soundIn 0
    x = sinOsc kr (1/4) 0
in xFade2 z (shaper (control kr "tbl" 10) z) x 0.5

-- shaper ; mouse control ; requires=tbl
let z = soundIn 0
    x = mouseX kr (-1) 1 Linear 0.2
in xFade2 z (shaper (control kr "tbl" 10) z) x 0.5

-- shaper ; event control
let f (_,g,_,y,z,o,_,_,p,_,_) =
      let s = sinOsc ar (unitCps p) 0 * y
          b = asLocalBuf (Gen.chebyShaperTbl 256 [1, 0, 1, 1, 0, 1])
      in pan2 (shaper b s) (o * 2 - 1) (z * g)
in mix (eventVoicer 16 f) * control kr "gain" 1.0

---- ; drawings
Sound.SC3.Plot.plot_p1_ln [Gen.cheby 256 [1, 0, 1, 1, 0, 1]]
Sound.SC3.Plot.plot_p1_ln [Gen.chebyShaperTbl 256 [1, 0, 1, 1, 0, 1]]
