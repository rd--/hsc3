-- crossoverDistortion
let x = mouseX kr 0 1 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
in X.crossoverDistortion (sinOsc ar (mce2 400 404) 0 * 0.2) x y

-- crossoverDistortion
let x = mouseX kr 0 1 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
in X.crossoverDistortion (soundIn 0) x y
