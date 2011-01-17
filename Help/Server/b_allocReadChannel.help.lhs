/b_allocReadChannel     Allocate buffer space and read channels from file

int    - buffer number
string - path name of a sound file.
int    - starting frame in file (optional. default = 0)
int    - number of frames to read (optional. default = 0, see below)
[int]  - source file channel indices
bytes  - an OSC message to execute upon completion. (optional)

As b_allocRead, but reads individual channels into the allocated
buffer in the order specified.

Asynchronous. Replies to sender with /done when complete.
