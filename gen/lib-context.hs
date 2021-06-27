import Sound.SC3.Common.Context {- hsc3 -}

main :: IO ()
main = do
  let dir = "/home/rohan/sw/hsc3/lib/"
  context_write (dir ++ "hsc3-min-imports.hs") min_context
  context_write (dir ++ "hsc3-std-imports.hs") std_context
