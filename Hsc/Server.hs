module Hsc.Server where

import Hsc.OpenSoundControl

type AddAction = Int

addToHead, addToTail, addBefore, addAfter, addReplace :: Int
addToHead  = 0
addToTail  = 1
addBefore  = 2
addAfter   = 3
addReplace = 4

quit            = OscM "/quit" []
notify c        = OscM "/notify" [OscInt c]
status          = OscM "/status" []
dumpOSC c       = OscM "/dumpOSC" [OscInt c]
sync id         = OscM "/sync" [OscInt id]
clearSched      = OscM "/clearSched" []

d_recv b        = OscM "/d_recv" [OscBlob b]
d_load p        = OscM "/d_load" [OscString p]
d_loadDir p     = OscM "/d_loafDir" [OscString p]
d_free n        = OscM "/d_free" [OscString n]

n_free id       = OscM "/n_free" [OscInt id]
n_run id f      = OscM "/n_run" [OscInt id, OscInt f]
n_set id i f    = OscM "/n_set" [OscInt id, OscString i, OscFloat f]
n_setn id i n l = OscM "/n_setn" $ [OscInt id, OscString i, OscInt n] ++ (map OscFloat l)
n_fill id i n v = OscM "/n_fill" [OscInt id, OscString i, OscInt n, OscFloat v]
n_map id i b    = OscM "/n_map" [OscInt id, OscString i, OscInt b]
n_mapn id i b n = OscM "/n_mapn" [OscInt id, OscString i, OscInt b, OscInt n]
n_before a b    = OscM "/n_before" [OscInt a, OscInt b]
n_query id      = OscM "/n_query" [OscInt id]
n_trace id      = OscM "/n_trace" [OscInt id]

s_new n i a t   = OscM "/s_new" [OscString n, OscInt i, OscInt a, OscInt t]
s_get id i      = OscM "/s_get" [OscInt id, OscInt i]
s_getn id i n   = OscM "/s_getn" [OscInt id, OscInt i, OscInt n]
s_noid id       = OscM "/s_noid" [OscInt id]

g_new id a t    = OscM "/g_new" [OscInt id, OscInt a, OscInt t]
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
b_setn id n l          = OscM "/b_setn" $ [OscInt id, OscInt n] ++ (map OscFloat l)
b_fill id i n f        = OscM "/b_fill" [OscInt id, OscInt i, OscInt n, OscFloat f]
b_close id             = OscM "/b_close" [OscInt id]
b_query id             = OscM "/b_query" [OscInt id]
b_get id i             = OscM "/b_get" [OscInt id, OscInt i]
b_getn id i n          = OscM "/b_getn" [OscInt id, OscInt i, OscInt n]

c_set  id f      = OscM "/c_set" [OscInt id, OscFloat f]
c_setn id n f    = OscM "/c_setn" $ [OscInt id, OscInt n] ++ (map OscFloat f)
c_fill id i f    = OscM "/c_fill" [OscInt id, OscInt i, OscFloat f]
c_get  id        = OscM "/c_get" [OscInt id]
c_getn id n      = OscM "/c_getn" [OscInt id, OscInt n]

-- Local Variables:
-- truncate-lines:t
-- End:
