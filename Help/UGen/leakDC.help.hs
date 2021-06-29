-- leakDC
let a = lfPulse ar 800 0 0.5 * 0.1 + 0.5
in mce [a,leakDC a 0.995]

---- ; frequency response functions and tables
---- ; <http://www.listarc.bham.ac.uk/lists/sc-users/msg69555.html>
leak_dc_coef sample_rate cfreq = exp (-two_pi * (cfreq / sample_rate))
leak_dc_cfreq sample_rate coef = -log(coef) * (sample_rate / two_pi)
import Sound.SC3.Plot {- hsc3-plot -}
plot_p2_ln [let x = [0,0.5 .. 200] in zip x (map (leak_dc_coef 48000) x)]
plot_p2_ln [let x = [0.9700,0.9725 .. 1.0000] in zip x (map (leak_dc_cfreq 48000) x)]
