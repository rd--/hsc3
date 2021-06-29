-- lpcAnalyzer
X.lpcAnalyzer (soundIn 0) (impulse ar 440 0 * 0.2) 256 50 0 0.999 0

-- lpcAnalyzer
X.lpcAnalyzer (soundIn 0) (impulse ar 440 0 * 0.2) 256 50 0 0.999 1

-- lpcAnalyzer
let x = mouseX kr 1 128 Linear 0.2
in X.lpcAnalyzer (soundIn 0) (impulse ar 440 0 * 0.2) 128 x 0 0.999 0

-- lpcAnalyzer
let x = mouseX kr 1 128 Linear 0.2
in X.lpcAnalyzer (soundIn 0) (impulse ar 440 0 * 0.2) 1024 x 0 0.999 1

-- lpcAnalyzer
let x = mouseX kr 1 256 Linear 0.2
in X.lpcAnalyzer (soundIn 0) (whiteNoise 'α' ar * 0.1) 256 x 0 0.999 0
