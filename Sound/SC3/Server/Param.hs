-- * Synthesis parameters
module Sound.SC3.Server.Param where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

-- | An SC3 synthesis parameters, ie. (control-name,control-value).
type Param1 = (String,Double)

-- | Set of SC3 synthesiser parameters.
type Param = [Param1]

-- | Add new, or overwrite existing, parameter.
param_insert :: Param -> Param1 -> Param
param_insert p z = z : deleteBy ((==) `on` fst) z p

-- | Merge, require keys be unique.
--
-- > param_merge_uniq [("a",1),("b",2)] [("c",3),("d",4)] == [("a",1),("b",2),("c",3),("d",4)]
-- > param_merge_uniq [("a",1)] [("a",2)] -- error
param_merge_uniq :: Param -> Param -> Param
param_merge_uniq p1 p2 =
  case map fst p1 `intersect` map fst p2 of
    [] -> p1 ++ p2
    _ -> error "param_merge_uniq?"

-- | Merge, right biased.
--
-- > param_merge_r [("a",1),("b",2)] [("c",3),("a",4)] == [("b",2),("c",3),("a",4)]
param_merge_r :: Param -> Param -> Param
param_merge_r p1 p2 =
    let p3 = let k2 = map fst p2 in filter (\(x,_) -> x `notElem` k2) p1
    in p3 ++ p2

-- | Right-fold (right-biased) of 'param_merge'
--
-- > param_merge_r_seq [[("a",1),("b",2)],[("c",3),("a",4)],[("b",5)]] == [("c",3),("a",4),("b",5)]
param_merge_r_seq :: [Param] -> Param
param_merge_r_seq = foldr1 param_merge_r

-- | Lookup parameter value, with default.
param_get :: Param -> String -> Double -> Double
param_get p k v = fromMaybe v (lookup k p)
