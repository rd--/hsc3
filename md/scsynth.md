# scsynth

Send control messages to `scsynth` and print replies.

## buffer

`buffer query` sends a `/b_query` message and prints the `/b_info`
reply.

~~~~
$ hsc3-scsynth buffer query 0
buffer-id      : 0
frame-count    : 256
channels-count : 1
sample-rate    : 48000.0
$
~~~~

`buffer store` writes the buffer contents to a `NeXT/AU` file.

`buffer store-seq` writes a sequence of time-stamped files to the indicated directory.

~~~~
$ hsc3-scsynth buffer store-seq 12 0.1 iso /tmp
^C
$ for i in /tmp/*.au ; do hsc3-sf-draw table pbm 0 12 128 0 $i $i.pbm ; done
$ convert /tmp/*.pbm -delay 1 -loop 0 ~/sw/hsc3-data/data/gif/monopole.gif
$
~~~~

![](sw/hsc3-data/data/gif/monopole.gif)

## node

`group query-tree` sends a `/g_queryTree` message and prints the
`/g_queryTree.reply`.

## node

`hsc3-scsynth node query` sends an `/n_query` message and prints the
`/n_info` reply.

## reset

`reset`, runs `/clearSched` and `/g_freeAll` and `/g_new`, `C-cC-k` in emacs.

## status

`status` sends a `/status` message and prints the `/status.reply`,
`C-cC-p` in emacs.

~~~~
$ hsc3-scsynth status
***** SuperCollider Server Status *****
# UGens                     219:Int32
# Synths                    5:Int32
# Groups                    3:Int32
# Instruments               22:Int32
% CPU (Average)             6.52046:Float
% CPU (Peak)                6.52434:Float
Sample Rate (Nominal)       48000.0:Double
Sample Rate (Actual)        47998.52114:Double
$
~~~~
