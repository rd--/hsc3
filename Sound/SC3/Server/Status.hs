module Sound.SC3.Server.Status where

import Sound.SC3.Server.Command (status)
import Sound.SC3.Server.OpenSoundControl (Osc(OscM), osc_show')
import Sound.SC3.Server.Udp (sync', sc, close')

import Data.List (transpose)
import Control.Exception (bracket)

statusFields :: [String]
statusFields = ["# UGens                     ", 
                "# Synths                    ", 
                "# Groups                    ",
                "# Instruments               ",
                "% CPU (Average)             ",
                "% CPU (Peak)                ",
                "Sample Rate (Nominal)       ",
                "Sample Rate (Actual)        "]

statusInfo :: Osc -> [String]
statusInfo (OscM "status.reply" l) = map osc_show' (tail l)
statusInfo _                       = error "non status.reply message"

status' :: IO Osc
status' = bracket sc close'
      (\fd ->
          do r <- sync' fd status
             putStrLn "***** SuperCollider Server Status *****"
             mapM putStr (concat (transpose [statusFields, statusInfo r, replicate 8 "\n"]))
             return r)
