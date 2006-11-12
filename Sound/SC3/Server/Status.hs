module Sound.SC3.Server.Status (status') where

import Sound.SC3.Server.Command (status)
import Sound.SC3.Server.OpenSoundControl (Osc(OscM), osc_show')
import Sound.SC3.Server.Udp (sync', close')

import Data.List (transpose)
import Control.Exception (bracket)
import Network.Socket (Socket)

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

statusFormat :: Osc -> [String]
statusFormat r = s : concat (transpose [statusFields, statusInfo r, replicate 8 "\n"])
    where s = "***** SuperCollider Server Status *****\n"

-- | Print server status information.
status' :: IO Socket -> IO Osc
status' sc = bracket sc close'
      (\fd ->
          do r <- sync' fd status
             mapM putStr (statusFormat r)
             return r)
