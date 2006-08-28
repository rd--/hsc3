module Hsc.Server where

import Hsc.OpenSoundControl
import Hsc.U8v (U8v)

data AddAction = AddToHead
               | AddToTail
               | AddBefore
               | AddAfter
               | AddReplace
                 deriving (Eq, Show, Enum)

hd, tl :: AddAction
hd = AddToHead
tl = AddToTail

addAction :: AddAction -> Int
addAction = fromEnum

quit            = OscM "/quit" []
notify c        = OscM "/notify" [OscInt c]
status          = OscM "/status" []
dumpOSC c       = OscM "/dumpOSC" [OscInt c]
sync id         = OscM "/sync" [OscInt id]
clearSched      = OscM "/clearSched" []

d_recv b        = OscM "/d_recv" [OscBlob b]
d_load p        = OscM "/d_load" [OscString p]
d_loadDir p     = OscM "/d_loadDir" [OscString p]
d_free n        = OscM "/d_free" [OscString n]

n_free id       = OscM "/n_free" [OscInt id]
n_run id f      = OscM "/n_run" [OscInt id, OscInt f]
n_set id i f    = OscM "/n_set" [OscInt id, OscString i, OscFloat f]
n_setn id i l   = OscM "/n_setn" $ [OscInt id, OscString i, OscInt (length l)] ++ (map OscFloat l)
n_fill id i n v = OscM "/n_fill" [OscInt id, OscString i, OscInt n, OscFloat v]
n_map id i b    = OscM "/n_map" [OscInt id, OscString i, OscInt b]
n_mapn id i b n = OscM "/n_mapn" [OscInt id, OscString i, OscInt b, OscInt n]
n_before a b    = OscM "/n_before" [OscInt a, OscInt b]
n_query id      = OscM "/n_query" [OscInt id]
n_trace id      = OscM "/n_trace" [OscInt id]

s_new n i a t   = OscM "/s_new" [OscString n, OscInt i, OscInt (addAction a), OscInt t]
s_get id i      = OscM "/s_get" [OscInt id, OscInt i]
s_getn id i n   = OscM "/s_getn" [OscInt id, OscInt i, OscInt n]
s_noid id       = OscM "/s_noid" [OscInt id]

g_new id a t    = OscM "/g_new" [OscInt id, OscInt (addAction a), OscInt t]
g_head g n      = OscM "/g_head" [OscInt g, OscInt n]
g_tail g n      = OscM "/g_tail" [OscInt g, OscInt n]
g_freeAll id    = OscM "/g_freeAll" [OscInt id]
g_deepFree id   = OscM "/g_deepFree" [OscInt id]

b_alloc id f c         = OscM "/b_alloc" [OscInt id, OscInt f, OscInt c]
b_allocRead id p f n   = OscM "/b_allocRead" [OscInt id, OscString p, OscInt f, OscInt n]
b_read id p f n f' z   = OscM "/b_read" [OscInt id, OscString p, OscInt f, OscInt n, OscInt f', OscInt z]
b_write id p h t f s z = OscM "/b_write" [OscInt id, OscString p, OscInt h, OscInt t, OscInt f, OscInt s, OscInt z]
b_free id              = OscM "/b_free" [OscInt id]
b_zero id              = OscM "/b_zero" [OscInt id]
b_set id i f           = OscM "/b_set" [OscInt id, OscInt i, OscFloat f]
b_setn id n l          = OscM "/b_setn" $ [OscInt id, OscInt n, OscInt (length l)] ++ (map OscFloat l)
b_fill id i n f        = OscM "/b_fill" [OscInt id, OscInt i, OscInt n, OscFloat f]
b_close id             = OscM "/b_close" [OscInt id]
b_query id             = OscM "/b_query" [OscInt id]
b_get id i             = OscM "/b_get" [OscInt id, OscInt i]
b_getn id i n          = OscM "/b_getn" [OscInt id, OscInt i, OscInt n]

c_set  id f      = OscM "/c_set" [OscInt id, OscFloat f]
c_setn id f      = OscM "/c_setn" $ [OscInt id, OscInt (length f)] ++ (map OscFloat f)
c_fill id i f    = OscM "/c_fill" [OscInt id, OscInt i, OscFloat f]
c_get  id        = OscM "/c_get" [OscInt id]
c_getn id n      = OscM "/c_getn" [OscInt id, OscInt n]

-- Variants with variable argument support.

flatten_controls = concatMap (\(name,val) -> [OscString name, OscFloat val])
n_set' id c      = OscM "/n_set" ([OscInt id] ++ flatten_controls c)
s_new' n i a t c = OscM "/s_new" ([OscString n, OscInt i, OscInt (addAction a), OscInt t] ++ flatten_controls c)

-- bundle = OscB 0 -- does not work :-(


b_alloc :: Int -> Int -> Int -> Osc
b_allocRead :: Int -> String -> Int -> Int -> Osc
b_close :: Int -> Osc
b_fill :: Int -> Int -> Int -> Double -> Osc
b_free :: Int -> Osc
b_get :: Int -> Int -> Osc
b_getn :: Int -> Int -> Int -> Osc
b_query :: Int -> Osc
b_read :: Int -> String -> Int -> Int -> Int -> Int -> Osc
b_set :: Int -> Int -> Double -> Osc
b_setn :: Int -> Int -> [Double] -> Osc
b_write :: Int -> String -> Int -> Int -> Int -> Int -> Int -> Osc
b_zero :: Int -> Osc
clearSched :: Osc
c_fill :: Int -> Int -> Double -> Osc
c_get :: Int -> Osc
c_getn :: Int -> Int -> Osc
c_set :: Int -> Double -> Osc
c_setn :: Int -> [Double] -> Osc
dumpOSC :: Int -> Osc
d_free :: String -> Osc
d_load :: String -> Osc
d_loadDir :: String -> Osc
d_recv :: U8v -> Osc
flatten_controls :: [(String, Double)] -> [OscT]
g_deepFree :: Int -> Osc
g_freeAll :: Int -> Osc
g_head :: Int -> Int -> Osc
g_new :: Int -> AddAction -> Int -> Osc
g_tail :: Int -> Int -> Osc
notify :: Int -> Osc
n_before :: Int -> Int -> Osc
n_fill :: Int -> String -> Int -> Double -> Osc
n_free :: Int -> Osc
n_map :: Int -> String -> Int -> Osc
n_mapn :: Int -> String -> Int -> Int -> Osc
n_query :: Int -> Osc
n_run :: Int -> Int -> Osc
n_set :: Int -> String -> Double -> Osc
n_setn :: Int -> String -> [Double] -> Osc
n_set' :: Int -> [(String, Double)] -> Osc
n_trace :: Int -> Osc
quit :: Osc
status :: Osc
sync :: Int -> Osc
s_get :: Int -> Int -> Osc
s_getn :: Int -> Int -> Int -> Osc
s_new :: String -> Int -> AddAction -> Int -> Osc
s_new' :: String
	  -> Int
	  -> AddAction
	  -> Int
	  -> [(String, Double)]
	  -> Osc
s_noid :: Int -> Osc

-- Local Variables:
-- truncate-lines:t
-- End:
