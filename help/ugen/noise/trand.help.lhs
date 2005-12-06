trand id lo hi trig

Generates a random float value in uniform distribution from lo each
time the trig signal changes from nonpositive to positive values

> let f = trand 0 KR (MCE [200,1600]) (MCE [500,3000]) (dust 0 KR (MCE [5,12]))
> in sinosc AR f 0 * 0.2
