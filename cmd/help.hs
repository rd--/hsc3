import System.Environment {- base -}

import Sound.SC3.UGen.DB {- hsc3-db -}

help :: [String]
help =
    ["hsc3-help command [arguments]"
    ," ugen-summary ugen-name..."]

main :: IO ()
main = do
  a <- getArgs
  case a of
    "ugen-summary":u -> mapM_ ugenSummary u
    _ -> putStrLn (unlines help)
