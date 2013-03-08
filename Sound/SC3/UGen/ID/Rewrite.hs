module Sound.SC3.UGen.ID.Rewrite where

import Data.Char {- base -}
import System.Directory {- directory -}

-- | Table of greek letters (upper-case,lower-case,name).
--
-- > length greek_letters == 24
-- > (['Α' .. 'Ρ'] ++ ['Σ' .. 'Ω']) == map (\(c,_,_) -> c) greek_letters
-- > (['α' .. 'ρ'] ++ ['σ' .. 'ω']) == map (\(_,c,_) -> c) greek_letters
greek_letters :: [(Char,Char,String)]
greek_letters =
    [('Α','α',"Alpha")
    ,('Β','β',"Beta")
    ,('Γ','γ',"Gamma")
    ,('Δ','δ',"Delta")
    ,('Ε','ε',"Epsilon")
    ,('Ζ','ζ',"Zeta")
    ,('Η','η',"Eta")
    ,('Θ','θ',"Theta")
    ,('Ι','ι',"Iota")
    ,('Κ','κ',"Kappa")
    ,('Λ','λ',"Lambda")
    ,('Μ','μ',"Mu")
    ,('Ν','ν',"Nu")
    ,('Ξ','ξ',"Xi")
    ,('Ο','ο',"Omicron")
    ,('Π','π',"Pi")
    ,('Ρ','ρ',"Rho")
    ,('Σ','σ',"Sigma")
    ,('Τ','τ',"Tau")
    ,('Υ','υ',"Upsilon")
    ,('Φ','φ',"Phi")
    ,('Χ','χ',"Chi")
    ,('Ψ','ψ',"Psi")
    ,('Ω','ω',"Omega")]

-- | Rewrite each haskell character literal at string /l/ with values
-- from the character supply /s/.
--
-- > putStrLn (rewrite ['α'..] "'a',' ','a'")
-- > > 'α',' ','β'
rewrite :: String -> String -> String
rewrite s l =
    case s of
      k:s' -> case l of
                [] -> []
                '\'' : c : '\'' : l' ->
                    if isLetter c
                    then '\'' : k : '\'' : rewrite s' l'
                    else '\'' : c : rewrite s ('\'' : l')
                c : l' -> c : rewrite s l'
      _ -> undefined

-- | Rewrite haskell character literals using greek letters as supply.
--
-- > putStrLn (hsc3_id_rewrite "'a','.','a'")
-- > > 'α','.','β'
hsc3_id_rewrite :: String -> String
hsc3_id_rewrite =
    let uc = map (\(c,_,_) -> c) greek_letters
        lc = map (\(_,c,_) -> c) greek_letters
    in rewrite (lc ++ uc)

-- | File based (haskell pre-processor) variant of 'hsc3_id_rewrite'.
hsc3_id_rewrite_preprocessor :: FilePath -> FilePath -> FilePath -> IO ()
hsc3_id_rewrite_preprocessor _ i_fn o_fn = do
  s <- readFile i_fn
  writeFile o_fn (hsc3_id_rewrite s)

-- | File based (inplace) variant of 'hsc3_id_rewrite'.  Copies file
-- to @~@ suffix and replaces initial file.
--
-- > let fn = "/home/rohan/sw/hsc3-graphs/gr/resonant-dust.hs"
-- > in hsc3_id_rewrite_file fn
hsc3_id_rewrite_inplace :: FilePath -> IO ()
hsc3_id_rewrite_inplace fn = do
  let fn' = fn ++ "~"
  copyFile fn fn'
  hsc3_id_rewrite_preprocessor fn fn' fn
