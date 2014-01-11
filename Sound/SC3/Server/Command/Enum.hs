-- | Enumeration of SC3 server commands.
module Sound.SC3.Server.Command.Enum where

import Data.Maybe {- base -}
import Sound.OSC.Type {- hosc -}

-- | SC3 server commands are strings.
type SC3_Command = String

-- | Enumerate server command numbers.
sc3_cmd_enumeration :: [(SC3_Command,Int)]
sc3_cmd_enumeration =
    [("/notify",1)
    ,("/status",2)
    ,("/quit",3)
    ,("/cmd",4)
    -- /d = synthdef
    ,("/d_recv",5)
    ,("/d_load",6)
    ,("/d_loadDir",7)
    ,("/d_freeAll",8)
    -- /s = synth
    ,("/s_new",9)
    -- /n = node
    ,("/n_trace",10)
    ,("/n_free",11)
    ,("/n_run",12)
    ,("/n_cmd",13)
    ,("/n_map",14)
    ,("/n_set",15)
    ,("/n_setn",16)
    ,("/n_fill",17)
    ,("/n_before",18)
    ,("/n_after",19)
    -- /u = ugen
    ,("/u_cmd",20)
    -- /g = group
    ,("/g_new",21)
    ,("/g_head",22)
    ,("/g_tail",23)
    ,("/g_freeAll",24)
    -- /c = control
    ,("/c_set",25)
    ,("/c_setn",26)
    ,("/c_fill",27)
    -- /b = buffer
    ,("/b_alloc",28)
    ,("/b_allocRead",29)
    ,("/b_read",30)
    ,("/b_write",31)
    ,("/b_free",32)
    ,("/b_close",33)
    ,("/b_zero",34)
    ,("/b_set",35)
    ,("/b_setn",36)
    ,("/b_fill",37)
    ,("/b_gen",38)
    --
    ,("/dumpOSC",39)
    -- _get
    ,("/c_get",40)
    ,("/c_getn",41)
    ,("/b_get",42)
    ,("/b_getn",43)
    ,("/s_get",44)
    ,("/s_getn",45)
    -- _query
    ,("/n_query",46)
    ,("/b_query",47)
    --
    ,("/n_mapn",48)
    ,("/s_noid",49)
    --
    ,("/g_deepFree",50)
    ,("/clearSched",51)
    --
    ,("/sync",52)
    --
    ,("/d_free",53)
    -- _channel
    ,("/b_allocReadChannel",54)
    ,("/b_readChannel",55)
    -- _tree
    ,("/g_dumpTree",56)
    ,("/g_queryTree",57)
    -- error
    ,("/error",58)
    -- _args
    ,("/s_newargs",59)
    --
    ,("/n_mapa",60)
    ,("/n_mapan",61)
    ,("/n_order",62)
    ]

-- | Lookup command number in 'sc3_cmd_enumeration'.
--
-- > map sc3_cmd_number ["/b_alloc","/s_new"] == [Just 28,Just 9]
sc3_cmd_number :: SC3_Command -> Maybe Int
sc3_cmd_number = flip lookup sc3_cmd_enumeration

-- | 'isJust' of 'sc3_cmd_number'.
known_sc3_cmd :: SC3_Command -> Bool
known_sc3_cmd = isJust . sc3_cmd_number

-- | List of asynchronous server commands.
async_cmds :: [SC3_Command]
async_cmds =
    ["/b_alloc"
    ,"/b_allocRead"
    ,"/b_allocReadChannel"
    ,"/b_close"
    ,"/b_free"
    ,"/b_read"
    ,"/b_readChannel"
    ,"/b_write"
    ,"/b_zero"
    ,"/d_load"
    ,"/d_loadDir"
    ,"/d_recv"
    ,"/notify"
    ,"/quit"
    ,"/sync"]

-- | 'True' if 'Message' is an asynchronous 'Message'.
--
-- > import Sound.SC3.Server.Command.Core
-- > map isAsync [b_close 0,n_set1 0 "0" 0] == [True,False]
isAsync :: Message -> Bool
isAsync (Message a _) = a `elem` async_cmds
