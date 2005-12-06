tirand id lo hi trig

Generates a random integer value in uniform distribution from lo to
hi each time the trig signal changes from nonpositive to positive
values

> pan2 AR (pinknoise 0 AR * 0.2) (tirand 0 KR (-1) 1 (dust 0 KR 10)) 1

> sinosc AR (tirand 0 KR 4 12 (dust 0 KR 10) * 150 + (MCE [0,1])) 0 * 0.1
