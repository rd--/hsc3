module Sound.SC3.Server.Status (serverStatus) where

import Sound.OpenSoundControl (OSC(Message))
import Sound.SC3.Server.Command (status)
import Sound.OpenSoundControl.TCP (TCP, send, wait)

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
statusFormat r = s : zipWith (++) statusFields (statusInfo r)
    where s = "***** SuperCollider Server Status *****"

-- | Collect server status information.
serverStatus :: TCP -> IO [String]
serverStatus fd = do send fd status 
                     r <- wait fd "status.reply"
                     return (statusFormat r)
