-- pause
let f = control kr "f" 440
    g = control kr "g" 1
in mrg [sinOsc ar f 0 * 0.1,pause g 1001]

---- ; control
g = mrg [sinOsc ar (control kr "f" 440) 0 * 0.1,pause (control kr "g" 1) 1001]
audition_at (1001,AddToTail,1,[]) g
audition_at (1002,AddToTail,1,[("f",880)]) g

---- ; request that node 1002 pause node 1001
withSc3 (Sound.OSC.sendMessage (n_set 1002 [("g",0)]))

---- ; restart node 1001
withSc3 (Sound.OSC.sendMessage (n_set 1002 [("g",1)]))
