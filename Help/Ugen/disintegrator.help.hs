-- disintegrator
let x = mouseX kr 0 1 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
    s = sinOsc ar (mce2 400 404) 0 * 0.05
in X.disintegratorId 'α' s x y
