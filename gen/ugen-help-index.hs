import Data.List {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Directory as T {- hmt -}

import Sound.SC3.Common.Math.Operator {- hsc3 -}

import Sound.SC3.UGen.DB {- hsc3-db -}
import Sound.SC3.UGen.DB.Rename {- hsc3-db -}

ugen_renamer :: String -> String
ugen_renamer u =
    case u of
      "in'" -> "in"
      _ -> u

gen_ln :: [FilePath] -> String -> String
gen_ln db u =
    let nm = ugen_renamer (fromSC3Name u)
        fn = nm ++ ".help.hs"
    in if fn `elem` db then printf "[%s](?t=hsc3&e=Help/UGen/%s)" nm fn else nm

ugen_blacklist :: [String]
ugen_blacklist =
    ["BasicOpUGen", "BinaryOpUGen", "UnaryOpUGen"
    ,"AbstractIn", "AbstractOut"
    ,"SharedIn", "SharedOut"
    ,"InBus" -- jitlib
    ,"BEQSuite","ChaosGen","Filter"]

gen_cat :: [FilePath] -> UGen_Cat_Table_Entry -> String
gen_cat db (c1,s) =
    let mk_hd = concat . maybe ["## ",c1] (\c2 -> ["## ",c1,":",c2])
        f (m_c2,u) = let u' = filter (`notElem` ugen_blacklist) u
                     in unlines [mk_hd m_c2,intercalate ",\n" (map (gen_ln db) u'),""]
    in unlines (map f s)

cat_blacklist :: [String]
cat_blacklist = ["Base","Input"]

cat_sub :: UGen_Cat_Table
cat_sub = filter ((`notElem` cat_blacklist) . fst) (ugen_categories_table sc3_ugen_cat_composite_x)

drop_last :: [a] -> [a]
drop_last = reverse . tail . reverse

cat_op :: UGen_Cat_Table
cat_op =
    let rw s = if last s == '_' then drop_last s else s
        nm = fromSC3Name . rw . show
    in [("Operator",[(Just "Binary",map nm [minBound :: SC3_Binary_Op .. maxBound])
                    ,(Just "Unary",map show [minBound :: SC3_Unary_Op .. maxBound])])]

main :: IO ()
main = do
  db <- T.dir_subset_rel [".hs"] "/home/rohan/sw/hsc3/Help/UGen/"
  let c = map (gen_cat db) (cat_sub ++ cat_op)
  writeFile "/home/rohan/sw/hsc3/Help/UGen/ix.md" (unlines c)
