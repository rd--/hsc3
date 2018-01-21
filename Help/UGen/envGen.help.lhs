    > Sound.SC3.UGen.Help.viewSC3Help "EnvGen"
    > Sound.SC3.UGen.DB.ugenSummary "EnvGen"

See also help files for the following envelope constructors:

- envADSR
- envASR
- envCoord
- envGate
- envLinen
- envPairs
- envPerc
- envSine
- envStep
- envTrapezoid
- envTriangle
- envXYC

> import Sound.SC3 {- hsc3 -}

env_circle joins the end of the envelope to the start

> e_01 :: Fractional n => Envelope n
> e_01 = Envelope [6000,700,100] [1,1] [EnvExp,EnvLin] Nothing Nothing 0

    import Sound.SC3.Plot {- hsc3-plot -}
    plotEnvelope [e_01]

> e_01_c :: Fractional n => Envelope n
> e_01_c = env_circle_0 e_01

    plotEnvelope [e_01_c]

> g_01 =
>     let f = envGen KR 1 1 0 1 DoNothing e_01_c
>     in sinOsc AR f 0 * 0.1 + impulse AR 1 0

Env([6000,700,100],[1,1],['exp','lin']).circle.asArray == [6000,2,-99,-99,700,1,2,0,100,1,1,0]

    > r = [0,4,3,0,6000,0,1,0,700,1,2,0,100,1,1,0,0,9e8,1,0]
    > envelope_sc3_array (env_circle e_01 0 EnvLin) == Just r

Env([0,1],[0.1]).asArray == [0,1,-99,-99,1,0.1,1,0]

    > e = Envelope [0,1] [0.1] [EnvLin] Nothing Nothing 0
    > envelope_sc3_array e == Just [0,1,-99,-99,1,0.1,1,0]

https://www.listarc.bham.ac.uk/lists/sc-users/msg14815.html

> e_02 =
>   let n = range 0.01 0.15 (lfNoise1 'α' KR 2)
>   in Envelope [0,1] [n] [EnvLin] Nothing (Just 0) 0

> e_02_c = env_circle_0 e_02

> g_02 =
>     let a = envGen AR 1 1 0 1 DoNothing e_02_c
>     in sinOsc AR (a * 400 + 500) 0 * 0.1

EnvGen used as non-linear Phasor, here the positive half of a sin
function is traversed more quickly than the negative half.

> e_03 :: (Floating n,Ord n) => Envelope n
> e_03 = envXYC [(0,0,EnvNum (-0.5)),(0.4,pi,EnvNum 0.5),(1,two_pi,EnvLin)]
> e_03_c = env_circle_0 e_03

    plotEnvelope [e_03]

> f_03 rt ts = sinOsc rt 0 (envGen rt 1 1 0 ts DoNothing e_03_c)

> g_03 = soundIn 0 * range 0.25 1 (f_03 KR 2)

   plot_ugen 0.1 (f_03 AR 0.1)
