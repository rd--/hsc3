module Hsc.Server where

import Hsc.OpenSoundControl

quit            = osc "/quit" []
notify c        = osc "/notify" [OscInt c]
status          = osc "/status" []
dumpOSC c       = osc "/dumpOSC" [OscInt c]
sync id         = osc "/sync" [OscInt id]
clearSched      = osc "/clearSched" []

d_recv b        = osc "/d_recv" [OscBlob b]
d_load p        = osc "/d_load" [OscString p]
d_loadDir p     = osc "/d_loafDir" [OscString p]
d_free n        = osc "/d_free" [OscString n]

n_free id       = osc "/n_free" [OscInt id]
n_run id f      = osc "/n_run" [OscInt id, OscInt f]
n_set id i f    = osc "/n_set" [OscInt id, OscString i, OscFloat f]
n_setn id i n l = osc "/n_setn" $ [OscInt id, OscString i, OscInt n] ++ (map OscFloat l)
n_fill id i n v = osc "/n_fill" [OscInt id, OscString i, OscInt n, OscFloat v]
n_map id i b    = osc "/n_map" [OscInt id, OscString i, OscInt b]
n_mapn id i b n = osc "/n_mapn" [OscInt id, OscString i, OscInt b, OscInt n]
n_before a b    = osc "/n_before" [OscInt a, OscInt b]
n_query id      = osc "/n_query" [OscInt id]
n_trace id      = osc "/n_trace" [OscInt id]

s_new n i a t   = osc "/s_new" [OscString n, OscInt i, OscInt a, OscInt t]
s_get id i      = osc "/s_get" [OscInt id, OscInt i]
s_getn id i n   = osc "/s_getn" [OscInt id, OscInt i, OscInt n]
s_noid id       = osc "/s_noid" [OscInt id]

g_new id a t    = osc "/g_new" [OscInt id, OscInt a, OscInt t]
g_head g n      = osc "/g_head" [OscInt g, OscInt n]
g_tail g n      = osc "/g_tail" [OscInt g, OscInt n]
g_freeAll id    = osc "/g_freeAll" [OscInt id]
g_deepFree id   = osc "/g_deepFree" [OscInt id]

b_alloc id f c         = osc "/b_alloc" [OscInt id, OscInt f, OscInt c]
b_allocRead id p f n   = osc "/b_allocRead" [OscInt id, OscString p, OscInt f, OscInt n]
b_read id p f n f' z   = osc "/b_read" [OscInt id, OscString p, OscInt f, OscInt n, OscInt f', OscInt z]
b_write id p h t f s z = osc "/b_write" [OscInt id, OscString p, OscInt h, OscInt t, OscInt f, OscInt s, OscInt z]
b_free id              = osc "/b_free" [OscInt id]
b_zero id              = osc "/b_zero" [OscInt id]
b_set id i f           = osc "/b_set" [OscInt id, OscInt i, OscFloat f]
b_setn id i n l        = osc "/b_setn" $ [OscInt id, OscInt n] ++ (map OscFloat l)
b_fill id i n f        = osc "/b_fill" [OscInt id, OscInt i, OscInt n, OscFloat f]
b_close id             = osc "/b_close" [OscInt id]
b_query id             = osc "/b_query" [OscInt id]
b_get id i             = osc "/b_get" [OscInt id, OscInt i]
b_getn id i n          = osc "/b_getn" [OscInt id, OscInt i, OscInt n]

c_set  id f      = osc "/c_set" [OscInt id, OscFloat f]
c_setn id n f    = osc "/c_setn" $ [OscInt id, OscInt n] ++ (map OscFloat f)
c_fill id i f    = osc "/c_fill" [OscInt id, OscInt i, OscFloat f]
c_get  id        = osc "/c_get" [OscInt id]
c_getn id n      = osc "/c_getn" [OscInt id, OscInt n]

-- Local Variables:
-- truncate-lines:t
-- End:
