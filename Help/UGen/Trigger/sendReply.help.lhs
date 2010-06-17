sendReply in replyID cmdName values

On receiving a trigger (0 to non-zero transition), send a trigger
message from the server back to all registered clients.  Clients
register by sending a /notify message to the server.

in      - the trigger

replyId - an integer that will be passed with the 
          trigger message.  This is useful if you
          have more than one SendReply in a SynthDef

cmdName - the name of the reply command to send

values  - a list of UGen or float values will be polled 
          at the time of trigger, and returned with the 
          trigger message

> import Sound.SC3.ID

> let { s0 = lfNoise0 'a' KR 5
>     ; s1 = lfNoise0 'b' KR 5
>     ; o = sinOsc AR (s0 * 200 + 500) 0 * s1 * 0.1 }
> in audition (mrg [sendReply s0 0 "/s-reply" [s0, s1], out 0 o])

> import Sound.OpenSoundControl

> withSC3 (\fd -> do { async fd (notify True)
>                    ; r <- wait fd "/s-reply"
>                    ; putStrLn (show r)
>                    ; async fd (notify False) })
