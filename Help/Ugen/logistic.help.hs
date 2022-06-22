-- logistic ; Sc3 default parameters
logistic ar 3 1000 0.5

-- logistic ; sample-rate
logistic ar 3.95 sampleRate 0.5

-- logistic ; onset of chaos
logistic ar (line kr 3.55 3.6 5 DoNothing) 1000 0.01 * 0.2

-- logistic ; mouse control
let x = mouseX kr 3 3.99 Linear 0.1
    y = mouseY kr 10 10000 Exponential 0.1
in logistic ar x y 0.25 * 0.2
