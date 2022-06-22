-- standardL
standardL ar (sampleRate / 2) 1.0 0.5 0 * 0.1

-- standardL ; vary frequency
standardL ar (mouseX kr 20 sampleRate Linear 0.2) 1 0.5 0 * 0.1

-- standardL ; mouse-controlled param
standardL ar (sampleRate / 2) (mouseX kr 0.9 4 Linear 0.2) 0.5 0 * 0.1

-- standardL ; frequency control
sinOsc ar (standardL ar 40 (mouseX kr 0.9 4 Linear 0.2) 0.5 0 * 800 + 900) 0 * 0.1
