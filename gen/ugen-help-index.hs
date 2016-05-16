import Data.List {- base -}
import Text.Printf {- base -}

import Sound.SC3.UGen.Name {- hsc3 -}

import Sound.SC3.UGen.DB {- hsc3-db -}

gen_ln :: String -> String
gen_ln u =
    let u' = fromSC3Name u
    in printf "[%s](?t=hsc3&e=Help/UGen/%s.help.lhs)" u' u'

gen_cat :: (String, [String]) -> String
gen_cat (c,u) =
    let Just c' = stripPrefix "UGens>" c
    in unlines ["## " ++ c',"",intercalate ",\n" (map gen_ln u),""]

main :: IO ()
main = do
  let c = map gen_cat ugen_categories_table
  writeFile "/home/rohan/sw/hsc3/Help/UGen/ix.md" (unlines c)
