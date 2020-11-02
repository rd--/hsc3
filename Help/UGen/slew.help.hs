-- slew
let z = lfPulse AR 800 0 0.5 * 0.1 in mce2 z (slew z 4000 4000)

-- slew
let z = saw AR 800 * 0.1 in mce2 z (slew z 400 400)

-- slew
let x = mouseX KR 200 12000 Exponential 0.2
    y = mouseY KR 200 12000 Exponential 0.2
in mce2 (slew (0 - saw AR 440) x y * 0.05) (slew (lfPulse AR 800 0 0.5) x y * 0.1)

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.025 (lfPulse AR 800 0 0.5)
Sound.SC3.Plot.plot_ugen1 0.025 (slew (lfPulse AR 800 0 0.5) 4000 4000)
Sound.SC3.Plot.plot_ugen1 0.025 (slew (lfPulse AR 800 0 0.5) 500 500)
