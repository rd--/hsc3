/d_loadDir                             Load directory of synth definitions

string - pathname of directory.
bytes  - an OSC message to execute upon completion. (optional)

Loads a directory of synth definitions. Resident definitions with the
same names are overwritten.

Asynchronous. Replies to sender with /done when complete.
