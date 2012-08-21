-- | Functions to provide mediated access to the SC3 help system.
module Sound.SC3.Server.Help where

import Control.Monad
import Sound.SC3.UGen.Help
import System.Cmd {- process -}
import System.FilePath {- filepath -}

-- | Generate path to indicated SC3 instance method help.
--
-- > let r = "./Reference/Server-Command-Reference.html#/b_alloc"
-- > in sc3HelpServerCommand "." "b_alloc" == r
sc3_server_command_ref :: FilePath -> String -> FilePath
sc3_server_command_ref d c =
    let f = d </> "Reference/Server-Command-Reference.html"
        c' = case c of
               '/':_ -> c
               _ -> '/':c
    in f ++ '#':c'

-- | Lookup @SC3@ help file for server command `c'.
--
-- > Sound.SC3.Server.Help.viewServerHelp "/b_allocRead"
-- > viewServerHelp "done"
viewServerHelp :: String -> IO ()
viewServerHelp c = do
  d <- sc3HelpDirectory
  let nm = sc3_server_command_ref d c
  br <- get_env_default "BROWSER" "x-www-browser"
  void (rawSystem br ["file://" ++ nm])
