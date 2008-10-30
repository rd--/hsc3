lfPulse rate freq iphase width

A non-band-limited pulse oscillator. Outputs a high value of one
and a low value of zero.  Note that the iphase argument was not
present in SC2.

freq - frequency in Hertz
iphase - initial phase offset in cycles ( 0..1 )
width - pulse width duty cycle from zero to one.

> audition (out 0 (lfPulse AR (lfPulse KR 3 0 0.3 * 200 + 200) 0 0.2 * 0.1))

> audition (out 0 (lfPulse AR 220 0 (mouseX KR 0 1 Linear 0.2) * 0.1))
