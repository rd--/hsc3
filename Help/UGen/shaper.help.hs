-- shaper ; hear waveshaper at pure (sin) tone ; requires=tbl see b_gen_cheby
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

-- shaper ; generate table at client and use localBuf
let z = sinOsc ar 300 0 * line kr 0 1 12 DoNothing
    c = Gen.cheby 257 [1,0,1,1,0,1]
    t = to_wavetable_nowrap c
    b = asLocalBuf 'α' t
in shaper b z * 0.1

-- shaper ; event control
let f _ (g,_,y,z,o,_,_,p,_,_) =
      let s = sinOsc ar (midiCPS p) 0 * y
          c = Gen.cheby 257 [1,0,1,1,0,1]
          b = asLocalBuf 'α' (to_wavetable_nowrap c)
      in pan2 (shaper b s) (o * 2 - 1) (z * g)
in mix (eventVoicer 16 f) * control kr "gain" 1.0

---- ; drawings
Sound.SC3.Plot.plot_p1_ln [Gen.cheby 257 [1,0,1,1,0,1]]
Sound.SC3.Plot.plot_p1_ln [to_wavetable_nowrap (Gen.cheby 257 [1,0,1,1,0,1])]
