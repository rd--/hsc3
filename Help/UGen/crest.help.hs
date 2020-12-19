-- crest ; mouse varies from sinewave (left) to almost-square (right)
let sig = (sinOsc AR (mouseY KR 100 1000 Exponential 0.1) 0 * mouseX KR 1 10 Linear 0.2) `clip2` 1
    val = X.crest KR sig 440 1
in mce2 sig (sinOsc AR (lag val 0.2 * 400) 0) * 0.1
