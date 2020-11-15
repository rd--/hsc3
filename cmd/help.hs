import System.Environment {- base -}

import qualified Sound.SC3.Lang.Help as Help {- hsc3-lang -}

import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}

{-

ugen-default-param-named =>
 let {gate=1.0, levelScale=1.0, levelBias=0.0, timeScale=1.0, doneAction=0.0, *envelope=0.0}
 in envGen AR gate levelScale levelBias timeScale doneAction envelope

ugen-default-record =>
 EnvGen {rate=AR, gate_=1, levelScale=1, levelBias=1, timeScale=1, doneAction=0, envelope_=0}

-}

-- > map ugen_default_param ["sinosc","whiteNoise","drand","pitch"]
ugen_default_param :: String -> String
ugen_default_param nm = maybe ("ERROR: NO ENTRY: " ++ nm) DB.u_default_param (DB.u_lookup_ci nm)

-- > map ugen_control_param ["sinosc","whiteNoise","drand","pitch"]
ugen_control_param :: String -> String
ugen_control_param nm = maybe ("ERROR: NO ENTRY: " ++ nm) DB.u_control_inputs_pp (DB.u_lookup_ci nm)

-- > map ugen_smalltalk ["sinosc","whiteNoise","drand","pitch"]
ugen_smalltalk :: String -> String
ugen_smalltalk nm = maybe ("ERROR: NO ENTRY: " ++ nm) DB.u_smalltalk_pp (DB.u_lookup_ci nm)

help :: [String]
help =
    ["hsc3-help command [arguments]"
    ," sc3-help {rtf|scdoc-local|scdoc-online} subject..."
    ," ugen-control-param ugen-name..."
    ," ugen-default-param ugen-name..."
    ," ugen-smalltalk ugen-name..."
    ," ugen-summary ugen-name..."]

main :: IO ()
main = do
  a <- getArgs
  case a of
    "sc3-help":"rtf":x -> mapM_ Help.sc3_rtf_help_scd_open_emacs x
    "sc3-help":"scdoc-local":x -> mapM_ (Help.sc3_scdoc_help_open True . Help.sc3_scdoc_help_path) x
    "sc3-help":"scdoc-online":x -> mapM_ (Help.sc3_scdoc_help_open False . Help.sc3_scdoc_help_path) x
    "ugen-control-param":u -> mapM_ (putStrLn . ugen_control_param) u
    "ugen-default-param":u -> mapM_ (putStrLn . ugen_default_param) u
    "ugen-smalltalk":u -> mapM_ (putStrLn . ugen_smalltalk) u
    "ugen-summary":u -> mapM_ DB.ugen_summary_wr u
    _ -> putStrLn (unlines help)
