-- | Request and display status information from the synthesis server.
module Sound.SC3.Server.Status where

import Data.Maybe {- base -}
import Sound.OSC.Type {- hosc -}

-- | Get /n/th field of status as 'Float'.
extractStatusField :: Floating n => Int -> [Datum] -> n
extractStatusField n =
    fromMaybe (error "extractStatusField")
    . datum_floating
    . (!! n)

-- | Names of status fields.
statusFields :: [String]
statusFields =
    ["Unused                      "
    ,"# UGens                     "
    ,"# Synths                    "
    ,"# Groups                    "
    ,"# Instruments               "
    ,"% CPU (Average)             "
    ,"% CPU (Peak)                "
    ,"Sample Rate (Nominal)       "
    ,"Sample Rate (Actual)        "]

-- | Status pretty printer.
statusFormat :: [Datum] -> [String]
statusFormat d =
    let s = "***** SuperCollider Server Status *****"
    in s : zipWith (++) (tail statusFields) (map show (tail d))
