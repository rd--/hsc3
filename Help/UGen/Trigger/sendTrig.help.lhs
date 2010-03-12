sendTrig in id value

On receiving a trigger (0 to non-zero transition), send a trigger
message from the server back to all registered clients.  Clients
register by sending a /notify message to the server.

input - the trigger

id    - an integer that will be passed with the trigger message.  This
  	is useful if you have more than one SendTrig in a SynthDef

value - a UGen or float that will be polled at the time of trigger,
        and its value passed with the trigger message

> import Sound.SC3.ID

> let { s = lfNoise0 'α' KR 5
>     ; o = sinOsc AR (s * 200 + 500) 0 * 0.1 }
> in audition (mrg [sendTrig s 0 s, out 0 o])

> import Sound.OpenSoundControl

> withSC3 (\fd -> do { async fd (notify True)
>                    ; tr <- wait fd "/tr"
>                    ; putStrLn (show tr)
>                    ; async fd (notify False) })
