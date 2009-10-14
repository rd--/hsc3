line rate start end dur doneAction

Generates a line from the start value to the end value.

start - starting value
end   - ending value
dur   - duration in seconds

Note: The SC3 UGen reorders the mul and add inputs to precede the
doneAction input.

> import Sound.SC3

> let f = line KR 200 17000 5 RemoveSynth
> in audition (out 0 (sinOsc AR f 0 * 0.1))
