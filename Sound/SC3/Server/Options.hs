-- | scsynth server command-line options.
module Sound.SC3.Server.Options where

import Data.List {- base -}

-- | (SHORT-OPTION,LONG-OPTION,DEFAULT-VALUE)
type SC3_OPT i = (Char,String,i)

-- | Get value from option.
sc3_opt_value :: SC3_OPT i -> i
sc3_opt_value (_,_,v) = v

-- | Default port number, either a 'u' or a 't' option is required.
sc3_port_def :: Num i => i
sc3_port_def = 57110

-- | Protocol is either UDP or TCP.
data SC3_PROTOCOL = SC3_UDP | SC3_TCP

-- | Default port option.
sc3_opt_port_def :: Num i => SC3_PROTOCOL -> SC3_OPT i
sc3_opt_port_def p =
  case p of
    SC3_UDP -> ('u',"udp-port-number",sc3_port_def)
    SC3_TCP -> ('t',"tcp-port-number",sc3_port_def)

-- | SC3 default options.
sc3_opt_def :: Num i => SC3_PROTOCOL -> [SC3_OPT i]
sc3_opt_def p =
  sc3_opt_port_def p :
  [('a',"number-of-audio-bus-channels",1024)
  ,('b',"number-of-sample-buffers",1024)
  ,('c',"number-of-control-bus-channels",16384)
  ,('D',"load-synthdefs?",1)
  ,('d',"max-number-of-synth-defs",1024)
  ,('i',"number-of-input-bus-channels",8)
  ,('l',"max-logins",64)
  ,('m',"real-time-memory-size",8192)
  ,('n',"max-number-of-nodes",1024)
  ,('o',"number-of-output-bus-channels",8)
  ,('r',"number-of-random-seeds",64)
  ,('R',"publish-to-rendezvous?",1)
  ,('S',"hardware-sample-rate",0)
  ,('V',"verbosity",0)
  ,('w',"number-of-wire-buffers",64)
  ,('z',"block-size",64)
  ,('Z',"hardware-buffer-size",0)]

-- | SC3 default options for UDP.
sc3_opt_def_udp :: Num i => [SC3_OPT i]
sc3_opt_def_udp = sc3_opt_def SC3_UDP

-- | Is option boolean, ie. 0=FALSE and 1=TRUE.
--
-- > filter sc3_opt_bool sc3_opt_def_udp
sc3_opt_bool :: SC3_OPT i -> Bool
sc3_opt_bool (_,s,_) = last s == '?'

-- | Lookup option given either short or long name.
sc3_opt_get :: [SC3_OPT i] -> Either Char String -> Maybe i
sc3_opt_get opt k =
  case k of
    Left c -> fmap sc3_opt_value (find (\(o,_,_) -> o == c) opt)
    Right s -> fmap sc3_opt_value (find (\(_,o,_) -> o == s) opt)

-- | Set option given either short or long name.
--
-- > sc3_opt_set sc3_opt_def_udp (Left 'w',256)
sc3_opt_set :: [SC3_OPT i] -> (Either Char String,i) -> [SC3_OPT i]
sc3_opt_set opt (k,v) =
  case k of
    Left x -> map (\(c,s,y) -> if c == x then (c,s,v) else (c,s,y)) opt
    Right x -> map (\(c,s,y) -> if s == x then (c,s,v) else (c,s,y)) opt

-- | Apply set of edits to options.
--
-- > sc3_opt_edit sc3_opt_def_udp [(Left 'w',256),(Left 'm',2 ^ 16)]
sc3_opt_edit :: [SC3_OPT i] -> [(Either Char String,i)] -> [SC3_OPT i]
sc3_opt_edit opt edt =
  case edt of
    [] -> opt
    x:rst -> sc3_opt_edit (sc3_opt_set opt x) rst

-- | Generate scsynth argument list.
--
-- > unwords (sc3_opt_arg sc3_opt_def_udp)
sc3_opt_arg :: Show i => [SC3_OPT i] -> [String]
sc3_opt_arg = concatMap (\(c,_,v) -> [['-',c],show v])

-- | Generate arguments for 'System.Process.callProcess' or related functions.
--
-- > sc3_opt_cmd sc3_opt_def_udp
sc3_opt_cmd :: Show i => [SC3_OPT i] -> (FilePath,[String])
sc3_opt_cmd opt = ("scsynth",sc3_opt_arg opt)
