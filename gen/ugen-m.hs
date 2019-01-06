import Data.List {- base -}
import Data.Maybe {- base -}

import Sound.SC3.UGen.DB {- hsc3-db -}
import Sound.SC3.UGen.DB.Record {- hsc3-db -}

is_filter :: U -> Bool
is_filter = isJust . ugen_filter

n_inputs :: U -> Int
n_inputs = length . ugen_inputs

gen_arg :: U -> String
gen_arg u = intercalate " -> " (replicate (n_inputs u + 1) "UGen m")

gen_sig :: String -> U -> String
gen_sig nm u =
  let rt = if is_filter u then "" else "Rate -> "
      md = if ugen_nondet u then "UId" else "Monad"
  in concat [nm," :: ",md," m => ",rt,gen_arg u]

gen_def :: String -> U -> String
gen_def nm u =
  let (nm',lift) = if ugen_nondet u
                   then (nm ++ "M","lift_ugenM_")
                   else (nm,"lift_ugen_")
  in if is_filter u
     then concat [nm," = ",lift,show (n_inputs u)," SC3.",nm']
     else concat [nm," rate = ",lift,show (n_inputs u)," (SC3.",nm', " rate)"]

gen_u :: String -> [String]
gen_u nm =
  let Just u = uLookup_ci nm
  in [gen_sig nm u,gen_def nm u]

{-
> u = ["allpassL","allpassN","combL","delayN","dust","lfNoise1","out","rand","resonz","sinOsc","whiteNoise"]
> mapM_ (putStrLn . unlines . gen_u) u
-}
