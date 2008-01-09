demandEnvGen rate levels times shapes curves gate reset
             levelScale levelOffset timeScale doneAction

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
         gate is x < 0, the ugen is sampled and held

reset  - if reset crosses from nonpositive to positive, the ugen
         is reset at the next level, if it is > 1, it is reset
         immediately.

Frequency ramp, exponential curve.

> do { l <- dseq 9E8 (mce [440, 9600])
>    ; let { y = mouseY KR 0.01 3 Exponential 0.1
>          ; f = demandEnvGen AR l y 2 0 1 1 1 0 1 DoNothing }
>      in audition (out 0 (sinOsc AR f 0 * 0.1)) }

Frequency envelope with random times.

> do { l <- dseq 9E8 (mce [204, 400, 201, 502, 300, 200])
>    ; t <- drand 9E8 (mce [1.01, 0.2, 0.1, 2.0])
>    ; let { y = mouseY KR 0.01 3 Exponential 0.1
>          ; f = demandEnvGen AR l (t * y) 7 0 1 1 1 0 1 DoNothing }
>      in audition (out 0 (sinOsc AR (f * mce [1, 1.01]) 0 * 0.1)) }
