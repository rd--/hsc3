-- sinGrain
X.sinGrain AR (impulse KR 10 0) 0.1 (range 440 880 (whiteNoise 'α' KR)) * 0.1

-- sinGrain ; mouse control
let x = mouseX KR 0.001 0.2 Linear 0.1
    y = mouseX KR 90 600 Linear 0.1
in X.sinGrain AR (dust 'β' KR 25) x y * 0.1
