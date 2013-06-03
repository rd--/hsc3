faust2supercollider zita_rev1.dsp
http://kokkinizita.linuxaudio.org/linuxaudio/zita-rev1-doc/quickguide.html

delay, lin,   0.02,    0.1,     0.04
xover, log,  50.0,  1000.0,   200.0
rtlow, log,   1.0,     8.0,     3.0
rtmid, log,   1.0,     8.0,     2.0
fdamp, log,   1.5e3,  24.0e3,   6.0e3
eq1fr, log,  40.0,     2.5e3, 160.0
eq1gn, lin, -15.0,    15.0,     0.0
eq2fr, log, 160.0,    10.0e3,   2.5e3
eq2gn, lin, -15.0,    15.0,     0.0
opmix, lin,   0.0,     1.0,     0.5
level, lin,  -9.0,     9.0,   -20.0

> import Sound.SC3

default settings
> let {i = soundIn 4
>     ;o = zitaRev1 i i 0.04 200 3 2 6000 160 0 2500 0 0.5 (-6)}
> in audition (out 0 o)

longer
> let {i = soundIn 4
>     ;o = zitaRev1 i i 0.08 200 6 4 6000 190 (-6) 3500 6 0.5 0}
> in audition (out 0 o)

longer still
> let {i = soundIn 4
>     ;o = zitaRev1 i i 0.1 200 6 8 6000 190 (-6) 3500 6 0.5 0}
> in audition (out 0 o)

hsc3-db
> Sound.SC3.UGen.DB.u_summary zitaRev1_dsc
