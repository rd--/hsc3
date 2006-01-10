coingate id prob in

When it receives a trigger, it tosses a coin, and either passes the
trigger or doesn't.

> let f = trand r0 KR 300.0 400.0 (coingate r0 KR 0.2 (impulse KR 10 0))
> in sinosc AR f 0 * 0.1
