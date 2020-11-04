-- blitB3Tri
X.blitB3Tri AR (xLine KR 1000 20 10 DoNothing) 0.99 0.99 * 0.1

unfortunately, aliasing returns at higher frequencies
(over 5000Hz or so) with a vengence (very beautiful in point scope)

> g_02 =
>   let x = mouseX KR 20 8000 Exponential 0.2
>       y = mouseY KR 0.001 0.99 Linear 0.2
>   in X.blitB3Tri AR x 0.99 y * 0.1

more efficient, some aliasing from 3000, but not so scary over 5000
Duller sound (less high harmonics included for lower fundamentals)

> g_03 =
>   let x = mouseX KR 20 8000 Exponential 0.2
>   in lfTri AR x 0 * 0.1
