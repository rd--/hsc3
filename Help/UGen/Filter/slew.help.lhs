slew in up dn

Has the effect of removing transients and higher frequencies.

> import Sound.SC3

> audition (out 0 (slew (saw AR 800 * 0.2) 400 400))
