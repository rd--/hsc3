-- disintegrator
let x = mouseX KR 0 1 Linear 0.2
    y = mouseY KR 0 1 Linear 0.2
    s = sinOsc AR (mce2 400 404) 0 * 0.05
in X.disintegrator 'α' s x y
