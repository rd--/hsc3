-- mouseButton ; amplitude envelope
sinOsc ar 800 0 * mouseButton kr 0 0.1 0.1

-- mouseButtonRand ; variant that randomly presses the button
sinOsc ar 800 0 * mouseButtonRand kr 0 0.1 0.1
