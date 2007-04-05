convolution in kernel frameSize

Strict convolution of two continuously changing inputs. Also see
[Convolution2] for a cheaper CPU cost alternative for the case of a
fixed kernel which can be changed with a trigger message.

in        - processing target
kernel    - processing kernel.
framesize - size of FFT frame, must be a power of two

> let i = in' 2 AR numOutputBuses
> k <- whiteNoise AR
> audition (out 0 (convolution i k 2048 * 0.1))
