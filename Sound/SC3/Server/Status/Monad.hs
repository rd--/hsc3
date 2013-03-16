-- | Request and display status information from the synthesis server.
module Sound.SC3.Server.Status.Monad where

import Control.Monad {- base -}
import Sound.OSC {- hosc -}

import Sound.SC3.Server.Command
import Sound.SC3.Server.Status

-- | Collect server status information.
serverStatus :: Transport m => m [String]
serverStatus = liftM statusFormat serverStatusData

-- | Read nominal sample rate of server.
serverSampleRateNominal :: (Transport m) => m Double
serverSampleRateNominal = liftM (extractStatusField 7) serverStatusData

-- | Read actual sample rate of server.
serverSampleRateActual :: (Transport m) => m Double
serverSampleRateActual = liftM (extractStatusField 8) serverStatusData

-- | Retrieve status data from server.
serverStatusData :: Transport m => m [Datum]
serverStatusData = do
  sendMessage status
  waitDatum "/status.reply"
