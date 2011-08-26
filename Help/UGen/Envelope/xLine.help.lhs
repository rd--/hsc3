xLine rate start end dur doneAction

Exponential line generator.  Generates an exponential curve from the
start value to the end value. Both the start and end values must be
non-zero and have the same sign.

start      - starting value
end        - ending value
dur        - duration in seconds
doneAction - a doneAction to be evaluated when the XLine is
             completed. See EnvGen for details.

SC3: at sclang mul and add inputs precede the doneAction input.

> import Sound.SC3

> let { f = xLine KR 200 17000 10 RemoveSynth
>     ; o = sinOsc AR f 0 * 0.1 }
> in audition (out 0 o)
