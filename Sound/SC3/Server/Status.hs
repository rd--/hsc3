-- | Request and display status information from the synthesis server.
module Sound.SC3.Server.Status where

import Sound.OSC.Type {- hosc -}

-- | Get /n/th field of status as 'Double'.
extractStatusField :: Int -> [Datum] -> Double
extractStatusField n = datum_real_err . (!! n)

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
