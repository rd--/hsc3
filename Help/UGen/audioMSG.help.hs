-- audioMSG
X.audioMSG AR (sinOsc AR 220 0 * 0.05) (mouseX KR 0 (2 * pi) Linear 0.2)

-- audioMSG ; warning=feedback
X.audioMSG AR (soundIn 0) (mouseX KR 0 (2 * pi) Linear 0.2)
