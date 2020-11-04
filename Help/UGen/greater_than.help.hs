-- greater_than ; c.f. equal_to ; trigger an envelope
let e = envGen KR (sinOsc AR 1 0 `greater_than` 0) 1 0 1 DoNothing (envPerc 0.01 1)
in sinOsc AR 440 0 * e * 0.1
