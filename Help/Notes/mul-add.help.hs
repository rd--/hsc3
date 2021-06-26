---- ; mulAdd ; optimisations

The order of the multiplier inputs is significant regards multi-rate inputs.

sclang re-orders these if required.

hsc3 does not.

The graph below will crash scsynth.

mulAdd 0.1 (sinOsc AR 440 0) 0.05

c.f.

{MulAdd.new(in: 0.1, mul: SinOsc.ar(freq: 440, phase: 0), add: 0.05)}.draw

Note: mulAdd with i-rate at input 0 and k-rate thereafter crashes scsynth.

sinOsc AR (mulAdd (control IR "x" 110) (control KR "y" 2) (control IR "z" 110)) 0 * 0.1

c.f.

{0.05 + (0.1 * SinOsc.ar(freq: 440, phase: 0))}.draw
