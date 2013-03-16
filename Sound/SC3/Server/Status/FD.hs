-- | Request and display status information from the synthesis server.
module Sound.SC3.Server.Status.FD where

import Control.Monad {- base -}
import Sound.OSC.FD {- hosc -}

import Sound.SC3.Server.Command
import Sound.SC3.Server.Status

-- | Collect server status information.
serverStatus :: Transport t => t -> IO [String]
serverStatus = liftM statusFormat . serverStatusData

-- | Read nominal sample rate of server.
serverSampleRateNominal :: (Transport t) => t -> IO Double
serverSampleRateNominal = liftM (extractStatusField 7) . serverStatusData

-- | Read actual sample rate of server.
serverSampleRateActual :: (Transport t) => t -> IO Double
serverSampleRateActual = liftM (extractStatusField 8) . serverStatusData

-- | Retrieve status data from server.
serverStatusData :: Transport t => t -> IO [Datum]
serverStatusData fd = do
  sendMessage fd status
  waitDatum fd "/status.reply"
