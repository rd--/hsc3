-- standardL
standardL AR (sampleRate / 2) 1.0 0.5 0 * 0.1

-- standardL ; vary frequency
standardL AR (mouseX KR 20 sampleRate Linear 0.2) 1 0.5 0 * 0.1

-- standardL ; mouse-controlled param
standardL AR (sampleRate / 2) (mouseX KR 0.9 4 Linear 0.2) 0.5 0 * 0.1

-- standardL ; frequency control
sinOsc AR (standardL AR 40 (mouseX KR 0.9 4 Linear 0.2) 0.5 0 * 800 + 900) 0 * 0.1
