mantissamask in bits

Masks off bits in the mantissa of the floating point sample
value. This introduces a quantization noise, but is less severe
than linearly quantizing the signal.

in - input signal
bits - the number of mantissa bits to preserve. a number from 0 to 23.

> mantissamask AR (sinosc AR (sinosc KR 0.2 0 * 400 + 500) 0 * 0.4) 3
