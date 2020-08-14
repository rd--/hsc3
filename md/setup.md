# hsc3-setup (May, 2015)

Co-ordination of [hsc3](http://rohandrape.net/t/hsc3) related packages.

There is a _make_ rule _build-setup_ that builds and installs the
_hsc3-setup_ executable, which depends only on standard haskell
packages (base, directory, filepath, process, split).

The package db is stored at `hsc3/db/setup.db`.

## echo

Print all projects in named category.

Categories are printed by `hsc3-setup -h`.

~~~~
hsc3-setup echo all
~~~~

## clone & update

Clone or update packages in _category_ from _source_ repositories to _destination_.

~~~~
hsc3-setup clone core http://rohandrape.net/sw /tmp
hsc3-setup update core http://rohandrape.net/sw ~/sw
~~~~

## local

Run a command at each local directory for _category_.

Common operations are to clean, check for local edits and push any changes to remote.

~~~~
hsc3-setup local all ~/sw cabal clean
hsc3-setup local all ~/sw darcs wh -ls
hsc3-setup local all ~/sw make push-rd
hsc3-setup local all ~/sw make mk-cmd
~~~~

# pkg-dep

Print package dependencies written as comments following import statements.

~~~~
$ for i in $(hsc3-setup pkg-dep -non-local ~/sw/hsc3-graphs/gr/*.hs) ; do echo $i ; done
MonadRandom
array
base
containers
directory
filepath
hashable
primes
process
random
random-shuffle
split
$
~~~~

# unregsiter & rebuild

~~~~
hsc3-setup unregister all
hsc3-setup rebuild all ~/sw
~~~~

# bootstrap

To setup all _hsc3_ packages on a new machine, given _darcs_ & _git_ & _ghc_ &
_cabal_, and setting LOCAL appropriately:

~~~~
LOCAL=...
REMOTE=http://rohandrape.net/sw
mkdir -p $LOCAL
cd $LOCAL
darcs get $REMOTE/hsc3/
(cd hsc3 ; make build-setup)
hsc3-setup clone all $REMOTE $LOCAL
hsc3-setup unregister all
hsc3-setup rebuild all $LOCAL
~~~~
