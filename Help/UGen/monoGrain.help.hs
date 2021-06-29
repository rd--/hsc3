-- monoGrain
let x = mouseX kr 0.1 0.01 Linear 0.2
    y = mouseX kr 10 4 Linear 0.2
in X.monoGrain ar (soundIn 0) x y 0
