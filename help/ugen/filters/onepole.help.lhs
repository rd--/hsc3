onepole in coef

A one pole filter.  Implements the formula: out(i) = ((1 -
abs(coef)) * in(i)) + (coef * out(i-1)).

in   - input signal to be processed
coef - feedback coefficient. Should be between -1 and +1

> onepole AR (whitenoise 0 AR * 0.5) 0.95

> onepole AR (whitenoise 0 AR * 0.5) (-0.95)

> onepole AR (whitenoise 0 AR * 0.5) (line KR (-0.99) 0.99 10 2)
