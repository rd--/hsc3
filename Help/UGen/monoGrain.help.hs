-- monoGrain
let x = mouseX KR 0.1 0.01 Linear 0.2
    y = mouseX KR 10 4 Linear 0.2
in X.monoGrain AR (soundIn 0) x y 0
