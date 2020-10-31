-- mouseButton ; amplitude envelope
sinOsc AR 800 0 * mouseButton KR 0 0.1 0.1

-- mouseButton ; variant that randomly presses the button
sinOsc AR 800 0 * mouseButton' KR 0 0.1 0.1
