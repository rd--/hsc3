import Data.List {- base -}
import Text.Printf {- base -}

import Sound.SC3.UGen.Name {- hsc3 -}

import Sound.SC3.UGen.DB {- hsc3-db -}

ugen_renamer :: String -> String
ugen_renamer u =
    case u of
      "in'" -> "in"
      _ -> u

gen_ln :: String -> String
gen_ln u =
    let u' = ugen_renamer (fromSC3Name u)
    in printf "[%s](?t=hsc3&e=Help/UGen/%s.help.lhs)" u' u'

ugen_blacklist :: [String]
ugen_blacklist =
    ["BasicOpUGen", "BinaryOpUGen", "UnaryOpUGen"
    ,"AbstractIn", "AbstractOut"
    ,"SharedIn", "SharedOut"
    ,"InBus" -- jitlib
    ,"Filter"]

gen_cat :: (String, [String]) -> String
gen_cat (c,u) =
    let Just c' = stripPrefix "UGens>" c
        u' = filter (`notElem` ugen_blacklist) u
    in unlines ["## " ++ c',"",intercalate ",\n" (map gen_ln u'),""]

cat_blacklist :: [String]
cat_blacklist = ["UGens>Base","UGens>Input"]

cat_sub :: [(String, [String])]
cat_sub = filter ((`notElem` cat_blacklist) . fst) ugen_categories_table

main :: IO ()
main = do
  let c = map gen_cat cat_sub
  writeFile "/home/rohan/sw/hsc3/Help/UGen/ix.md" (unlines c)
