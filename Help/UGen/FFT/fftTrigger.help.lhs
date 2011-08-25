fftTrigger buffer hop=0.5 polar=0

Outputs the necessary signal for FFT chains, without doing an FFT on a
signal.

buffer - a buffer to condition for FFT use
hop - the hop size for timing triggers (defaults to 0.5)
polar - if 0 prepare for complex data, if >0 prepare for polar data
