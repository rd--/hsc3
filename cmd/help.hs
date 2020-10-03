import System.Environment {- base -}

import qualified Sound.SC3.Lang.Help as Help {- hsc3-lang -}

import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}

help :: [String]
help =
    ["hsc3-help command [arguments]"
    ," sc3-help {rtf|scdoc-local|scdoc-online} subject..."
    ," ugen-summary ugen-name..."]

main :: IO ()
main = do
  a <- getArgs
  case a of
    "sc3-help":"rtf":x -> mapM_ Help.sc3_rtf_help_scd_open_emacs x
    "sc3-help":"scdoc-local":x -> mapM_ (Help.sc3_scdoc_help_open True . Help.sc3_scdoc_help_path) x
    "sc3-help":"scdoc-online":x -> mapM_ (Help.sc3_scdoc_help_open False . Help.sc3_scdoc_help_path) x
    "ugen-summary":u -> mapM_ DB.ugenSummary u
    _ -> putStrLn (unlines help)
