-- crest ; mouse varies from sinewave (left) to almost-square (right)
let sig = (sinOsc ar (mouseY kr 100 1000 Exponential 0.1) 0 * mouseX kr 1 10 Linear 0.2) `clip2` 1
    val = X.crest kr sig 440 1
in mce2 sig (sinOsc ar (lag val 0.2 * 400) 0) * 0.1
