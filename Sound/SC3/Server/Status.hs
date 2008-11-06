-- | Request and display status information from the synthesis server.
module Sound.SC3.Server.Status ( serverStatus
                               , serverSampleRateNominal
                               , serverSampleRateActual ) where

import Control.Monad
import Sound.OpenSoundControl
import Sound.SC3.Server.Command

-- | Collect server status information.
serverStatus :: Transport t => t -> IO [String]
serverStatus fd = liftM statusFormat (serverStatusData fd)

-- | Read nominal sample rate of server.
serverSampleRateNominal :: (Transport t) => t -> IO Double
serverSampleRateNominal fd = liftM (extractDouble . (!! 7)) (serverStatusData fd)

-- | Read actual sample rate of server.
serverSampleRateActual :: (Transport t) => t -> IO Double
serverSampleRateActual fd = liftM (extractDouble . (!! 8)) (serverStatusData fd)

extractDouble :: Datum -> Double
extractDouble (Float f) = f
extractDouble (Double f) = f
extractDouble _ = error "extractDouble"

serverStatusData :: Transport t => t -> IO [Datum]
serverStatusData fd = do send fd status 
                         (Message _ d) <- wait fd "status.reply"
                         return d

statusFields :: [String]
statusFields = ["Unused                      ",
                "# UGens                     ", 
                "# Synths                    ", 
                "# Groups                    ",
                "# Instruments               ",
                "% CPU (Average)             ",
                "% CPU (Peak)                ",
                "Sample Rate (Nominal)       ",
                "Sample Rate (Actual)        "]

statusFormat :: [Datum] -> [String]
statusFormat d = s : zipWith (++) 
                             (tail statusFields) 
                             (map show (tail d))
    where s = "***** SuperCollider Server Status *****"
