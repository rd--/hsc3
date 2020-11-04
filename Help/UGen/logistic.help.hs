-- logistic ; SC3 default parameters
logistic AR 3 1000 0.5

-- logistic ; sample-rate
logistic AR 3.95 sampleRate 0.5

-- logistic ; onset of chaos
logistic AR (line KR 3.55 3.6 5 DoNothing) 1000 0.01 * 0.2

-- logistic ; mouse control
let x = mouseX KR 3 3.99 Linear 0.1
    y = mouseY KR 10 10000 Exponential 0.1
in logistic AR x y 0.25 * 0.2
