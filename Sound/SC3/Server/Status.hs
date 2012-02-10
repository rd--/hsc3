-- | Request and display status information from the synthesis server.
module Sound.SC3.Server.Status (serverStatus
                               ,serverSampleRateNominal
                               ,serverSampleRateActual) where

import Control.Monad
import Sound.OpenSoundControl
import Sound.SC3.Server.Command

-- | Collect server status information.
serverStatus :: Transport t => t -> IO [String]
serverStatus = liftM statusFormat . serverStatusData

-- | Read nominal sample rate of server.
serverSampleRateNominal :: (Transport t) => t -> IO Double
serverSampleRateNominal = liftM (extractDouble . (!! 7)) . serverStatusData

-- | Read actual sample rate of server.
serverSampleRateActual :: (Transport t) => t -> IO Double
serverSampleRateActual = liftM (extractDouble . (!! 8)) . serverStatusData

extractDouble :: Datum -> Double
extractDouble d =
    case d of
      Float f -> f
      Double f -> f
      _ -> error "extractDouble"

serverStatusData :: Transport t => t -> IO [Datum]
serverStatusData fd = do
  send fd status
  (Message _ d) <- wait fd "/status.reply"
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
statusFormat d =
    let s = "***** SuperCollider Server Status *****"
    in s : zipWith (++) (tail statusFields) (map show (tail d))
