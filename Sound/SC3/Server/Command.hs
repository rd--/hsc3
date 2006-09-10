module Sound.SC3.Server.Command where

import Sound.SC3.Server.OpenSoundControl
import Sound.SC3.Server.U8v (U8)

data AddAction = AddToHead
               | AddToTail
               | AddBefore
               | AddAfter
               | AddReplace
                 deriving (Eq, Show, Enum)

addAction :: AddAction -> Int
addAction = fromEnum

quit            = OscM "/quit" []
notify c        = OscM "/notify" [OscInt c]
status          = OscM "/status" []
dumpOSC c       = OscM "/dumpOSC" [OscInt c]
sync nid        = OscM "/sync" [OscInt nid]
clearSched      = OscM "/clearSched" []

d_recv b        = OscM "/d_recv" [OscBlob b]
d_load p        = OscM "/d_load" [OscString p]
d_loadDir p     = OscM "/d_loadDir" [OscString p]
d_free n        = OscM "/d_free" [OscString n]

n_free nid       = OscM "/n_free" [OscInt nid]
n_run nid f      = OscM "/n_run" [OscInt nid, OscInt f]
n_set nid i f    = OscM "/n_set" [OscInt nid, OscString i, OscFloat f]
n_setn nid i l   = OscM "/n_setn" $ [OscInt nid, OscString i, OscInt (length l)] ++ (map OscFloat l)
n_fill nid i n v = OscM "/n_fill" [OscInt nid, OscString i, OscInt n, OscFloat v]
n_map nid i b    = OscM "/n_map" [OscInt nid, OscString i, OscInt b]
n_mapn nid i b n = OscM "/n_mapn" [OscInt nid, OscString i, OscInt b, OscInt n]
n_before a b     = OscM "/n_before" [OscInt a, OscInt b]
n_query nid      = OscM "/n_query" [OscInt nid]
n_trace nid      = OscM "/n_trace" [OscInt nid]

s_new n i a t    = OscM "/s_new" [OscString n, OscInt i, OscInt (addAction a), OscInt t]
s_get nid i      = OscM "/s_get" [OscInt nid, OscInt i]
s_getn nid i n   = OscM "/s_getn" [OscInt nid, OscInt i, OscInt n]
s_noid nid       = OscM "/s_noid" [OscInt nid]

g_new nid a t    = OscM "/g_new" [OscInt nid, OscInt (addAction a), OscInt t]
g_head g n       = OscM "/g_head" [OscInt g, OscInt n]
g_tail g n       = OscM "/g_tail" [OscInt g, OscInt n]
g_freeAll nid    = OscM "/g_freeAll" [OscInt nid]
g_deepFree nid   = OscM "/g_deepFree" [OscInt nid]

b_alloc nid f c         = OscM "/b_alloc" [OscInt nid, OscInt f, OscInt c]
b_allocRead nid p f n   = OscM "/b_allocRead" [OscInt nid, OscString p, OscInt f, OscInt n]
b_read nid p f n f' z   = OscM "/b_read" [OscInt nid, OscString p, OscInt f, OscInt n, OscInt f', OscInt z]
b_write nid p h t f s z = OscM "/b_write" [OscInt nid, OscString p, OscInt h, OscInt t, OscInt f, OscInt s, OscInt z]
b_free nid              = OscM "/b_free" [OscInt nid]
b_zero nid              = OscM "/b_zero" [OscInt nid]
b_set nid i f           = OscM "/b_set" [OscInt nid, OscInt i, OscFloat f]
b_setn nid n l          = OscM "/b_setn" $ [OscInt nid, OscInt n, OscInt (length l)] ++ (map OscFloat l)
b_fill nid i n f        = OscM "/b_fill" [OscInt nid, OscInt i, OscInt n, OscFloat f]
b_close nid             = OscM "/b_close" [OscInt nid]
b_query nid             = OscM "/b_query" [OscInt nid]
b_get nid i             = OscM "/b_get" [OscInt nid, OscInt i]
b_getn nid i n          = OscM "/b_getn" [OscInt nid, OscInt i, OscInt n]

c_set  nid f      = OscM "/c_set" [OscInt nid, OscFloat f]
c_setn nid f      = OscM "/c_setn" $ [OscInt nid, OscInt (length f)] ++ (map OscFloat f)
c_fill nid i f    = OscM "/c_fill" [OscInt nid, OscInt i, OscFloat f]
c_get  nid        = OscM "/c_get" [OscInt nid]
c_getn nid n      = OscM "/c_getn" [OscInt nid, OscInt n]

-- Variants with variable argument support.

flatten_controls  = concatMap (\(name,val) -> [OscString name, OscFloat val])
n_set' nid c      = OscM "/n_set" ([OscInt nid] ++ flatten_controls c)
s_new' n i a t c  = OscM "/s_new" ([OscString n, OscInt i, OscInt (addAction a), OscInt t] ++ flatten_controls c)

-- Sig.

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
d_recv :: [U8] -> Osc
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
s_new' :: String -> Int -> AddAction -> Int -> [(String, Double)] -> Osc
s_noid :: Int -> Osc

-- Local Variables:
-- truncate-lines:t
-- End:
