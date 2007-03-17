lagIn numChannels bus lag

Smooth a control rate input signal.

> withSC3 (\fd -> do send fd (c_set [(10, 200)])
>                    play fd (sinOsc AR (lagIn 1 10 1) 0 * 0.1)
>                    threadDelay 500000
>                    send fd (c_set [(10, 2000)]))
