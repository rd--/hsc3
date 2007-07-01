DemandEnvGen rate levels times shapes curves gate reset levelScale levelOffset timeScale doneAction

levels - a demand ugen or any other ugen

times  - a demand ugen or any other ugen if one of these ends,
         the doneAction is evaluated

shapes - a demand ugen or any other ugen, the number given is
         the shape number according to Env

curves - a demand ugen or any other ugen, if shape is 5, this
         is the curve factor some curves/shapes don't work if
         the duration is too short. have to see how to improve
         this. also some depend on the levels obviously, like
         exponential cannot cross zero.

gate   - if gate is x >= 1, the ugen runs, if gate is 0 > x > 1,
         the ugen is released at the next level (doneAction), if
         gate is x < 0, the ugen is sampled end held

reset  - if reset crosses from nonpositive to positive, the ugen
         is reset at the next level, if it is > 1, it is reset
         immediately.

Frequency envelope with random times.

> let inf_sc = 9e8
> l <- dseq inf_sc (MCE [204, 400, 201, 502, 300, 200])
> t <- drand inf_sc (MCE [1.01, 0.2, 0.1, 2.0])
> let y = mouseY KR 0.01 3 Exponential 0.1
>     f = demandEnvGen AR l (t * y) 7 0 1 1 1 0 1 DoNothing
> audition (out 0 (sinOsc AR (f * MCE [1, 1.01]) 0 * 0.1))
