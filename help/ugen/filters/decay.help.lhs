decay in decayTime

Exponential decay.  This is essentially the same as Integrator
except that instead of supplying the coefficient directly, it is
caculated from a 60 dB decay time. This is the time required for
the integrator to lose 99.9 % of its value or -60dB. This is useful
for exponential decaying envelopes triggered by impulses.

Used as an envelope.

> decay AR (impulse AR (xline KR 1 50 20 2) 0.25) 0.2 * (pinknoise 0 AR)
