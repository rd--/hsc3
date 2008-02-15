module Sound.SC3.Server.Status (serverStatus) where

import Sound.OpenSoundControl
import Sound.SC3.Server.Command (status)

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
statusInfo o = maybe [] f (address o)
    where f a = if a == "status.reply" 
                then maybe [] (map show . tail) (arguments o) 
                else []

statusFormat :: OSC -> [String]
statusFormat r = s : zipWith (++) statusFields (statusInfo r)
    where s = "***** SuperCollider Server Status *****"

-- | Collect server status information.
serverStatus :: Transport t => t -> IO [String]
serverStatus fd = do send fd status 
                     r <- wait fd "status.reply"
                     return (statusFormat r)
