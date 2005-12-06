linrand id lo hi minmax

Generates a single random float value in linear distribution from
lo to hi, skewed towards lo if minmax < 0, otherwise skewed towards
hi.

> fsinosc AR (linrand 0 IR 200.0 10000.0 (MCE [-1, 1])) 0 * line KR 0.4 0 0.01 2
