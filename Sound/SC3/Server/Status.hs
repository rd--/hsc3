-- | Request and display status information from the synthesis server.
module Sound.SC3.Server.Status where

import Control.Monad
import Sound.OpenSoundControl
import Sound.SC3.Server.Command

-- | Collect server status information.
serverStatus :: Transport t => t -> IO [String]
serverStatus = liftM statusFormat . serverStatusData

-- | Read nominal sample rate of server.
serverSampleRateNominal :: (Transport t) => t -> IO Double
serverSampleRateNominal = liftM (extractStatusField 7) . serverStatusData

-- | Read actual sample rate of server.
serverSampleRateActual :: (Transport t) => t -> IO Double
serverSampleRateActual = liftM (extractStatusField 8) . serverStatusData

-- | Get /n/th field of status as 'Double'.
extractStatusField :: Int -> [Datum] -> Double
extractStatusField n = datum_real_err . (!! n)

-- | Retrieve status data from server.
serverStatusData :: Transport t => t -> IO [Datum]
serverStatusData fd = do
  sendMessage fd status
  Message _ d <- waitMessage fd "/status.reply"
  return d

-- | Names of status fields.
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

-- | Status pretty printer.
statusFormat :: [Datum] -> [String]
statusFormat d =
    let s = "***** SuperCollider Server Status *****"
    in s : zipWith (++) (tail statusFields) (map show (tail d))
