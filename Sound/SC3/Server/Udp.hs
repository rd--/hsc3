module Sound.SC3.Server.Udp where

import Sound.OpenSoundControl.U8v (u8v_str, str_u8v)
import Sound.OpenSoundControl (OSC, encode, decode)

import Network.Socket

sc :: IO Socket
sc = do fd <- socket AF_INET Datagram 0
        a <- inet_addr "127.0.0.1"
        connect fd (SockAddrInet 57110 a)
        -- setSocketOption fd RecvTimeOut 1000
        return fd

send' :: Socket -> OSC -> IO Int
send' fd o = send fd (u8v_str (encode o))

recv' :: Socket -> IO OSC
recv' fd   = do b <- recv fd 8192
                return (decode (str_u8v b))

sync' :: Socket -> OSC -> IO OSC
sync' fd o = do send' fd o
                recv' fd

close' :: Socket -> IO ()
close' = sClose
