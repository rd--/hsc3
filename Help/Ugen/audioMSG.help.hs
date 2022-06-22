-- audioMSG
X.audioMSG (sinOsc ar 220 0 * 0.05) (mouseX kr 0 (2 * pi) Linear 0.2)

-- audioMSG ; warning=feedback
X.audioMSG (soundIn 0) (mouseX kr 0 (2 * pi) Linear 0.2)
