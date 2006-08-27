module Hsc.Status where

import Hsc.Server (status)
import Hsc.OpenSoundControl (Osc(OscM), osc_show')
import Hsc.Udp (sync', sc, close')
import Hsc.List (interleave)
import Control.Exception (bracket)

statusFields = ["# UGens                     ", 
                "# Synths                    ", 
                "# Groups                    ",
                "# Instruments               ",
                "% CPU (Average)             ",
                "% CPU (Peak)                ",
                "Sample Rate (Nominal)       ",
                "Sample Rate (Actual)        "]

statusInfo (OscM "status.reply" l) = map osc_show' (tail l)
statusInfo _                       = error "non status.reply message"

status' = bracket sc close'
      (\fd ->
          do r <- sync' fd status
             putStrLn "***** SuperCollider Server Status *****"
             mapM putStr (interleave [statusFields, statusInfo r, replicate 8 "\n"])
             return r)
