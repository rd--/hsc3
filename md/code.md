# code

The `pkg-dep` sub-command prints package dependencies written as
comments following import statements.

~~~~
$ for i in $(hsc3-code pkg-dep ~/sw/hsc3-graphs/gr/*.hs) ; do echo $i ; done
MonadRandom
array
base
binary
bytestring
cairo
containers
directory
filepath
hashable
hls
hmt
hosc
hps
hsc3
hsc3-cairo
hsc3-lang
hsc3-rec
hsc3-sf
hsc3-unsafe
hsharc
primes
process
random
random-shuffle
sc3-rdu
split
$
~~~~
