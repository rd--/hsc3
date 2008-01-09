sendTrig in id value

On receiving a trigger (0 to non-zero transition), send a trigger
message from the server back to all registered clients.  Clients
register by sending a /notify message to the server.

input - the trigger

id    - an integer that will be passed with the trigger message.  This
  	is useful if you have more than one SendTrig in a SynthDef

value - a UGen or float that will be polled at the time of trigger,
        and its value passed with the trigger message

> withSC3 (\fd -> do { send fd (notify True)
>                    ; wait fd "/done" })

> do { s <- lfNoise0 KR 10
>    ; let o = sinOsc AR (s * 200 + 500) 0 * 0.1
>      in audition (mrg [sendTrig s 0 s, out 0 o]) }

> withSC3 (\fd -> do { tr <- wait fd "/tr"
>                    ; putStrLn (show tr) })

> withSC3 (\fd -> do { send fd (notify False)
>                    ; wait fd "/done" })
