module Sound.SC3.UGen.ID.Rewrite (hsc3_id_rewrite) where

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
-- > putStrLn (rewrite ['α'..] "'a','a'")
-- > > 'α','β'
rewrite :: String -> String -> String
rewrite s l =
    case s of
      k:s' -> case l of
                [] -> []
                '\'' : _ : '\'' : l' -> '\'' : k : '\'' : rewrite s' l'
                c : l' -> c : rewrite s l'
      _ -> undefined

-- | Rewrite haskell character literals using greek letters as supply.
--
-- > putStrLn (hsc3_id_rewrite "'.','.'")
-- > > 'α','β'
hsc3_id_rewrite :: String -> String
hsc3_id_rewrite =
    let uc = map (\(c,_,_) -> c) greek_letters
        lc = map (\(_,c,_) -> c) greek_letters
    in rewrite (lc ++ uc)
