module Sound.SC3.Server.Status (status') where

import Sound.OpenSoundControl (OSC(Message))
import Sound.SC3.Server.Command (status)
import Sound.OpenSoundControl.UDP (UDP, send, wait)

import Data.List (transpose)

statusFields :: [String]
statusFields = ["# UGens                     ", 
                "# Synths                    ", 
                "# Groups                    ",
                "# Instruments               ",
                "% CPU (Average)             ",
                "% CPU (Peak)                ",
                "Sample Rate (Nominal)       ",
                "Sample Rate (Actual)        "]

statusInfo :: OSC -> [String]
statusInfo (Message "status.reply" l) = map show (tail l)
statusInfo _                          = error "non status.reply message"

statusFormat :: OSC -> [String]
statusFormat r = s : concat (transpose [statusFields, statusInfo r, replicate 8 "\n"])
    where s = "***** SuperCollider Server Status *****\n"

-- | Print server status information.
status' :: UDP -> IO OSC
status' fd = do send fd status 
                r <- wait fd "status.reply"
                mapM putStr (statusFormat r)
                return r
