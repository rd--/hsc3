-- vbSlide
let src = lfNoise0 'α' AR 3 `in_range` (0,1)
    flt = X.vbSlide src 500 1500
    gen x = sinOsc AR (midiCPS (x `in_range` (30,80))) 0 * 0.1
in mce2 (gen flt) (gen src)

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
plot_ugen_nrt (48000,64) 0.07 (let src = lfPulse AR 10 0 0.2 in mce2 src (X.vbSlide src 100 500))

