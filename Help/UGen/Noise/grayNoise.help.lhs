grayNoise

Generates noise which results from flipping random bits in a word.
This type of noise has a high RMS level relative to its peak to
peak level.  The spectrum is emphasized towards lower frequencies.

> audition . (* 0.1) =<< grayNoise AR
