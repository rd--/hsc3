lag in lagTime

A simple averaging filter.

> let x = mousex KR 220 440 0 0.2
> in sinosc AR (MCE [x, lag KR x 1]) 0 * 0.1
