-- mouseX ; frequency control
let x = mouseX KR 40 10000 Exponential 0.2
in sinOsc AR x 0 * 0.1

-- mouseX ; variant with equal arguments but random traversal
let x = mouseX' KR 40 10000 Exponential 0.2
in sinOsc AR x 0 * 0.1
