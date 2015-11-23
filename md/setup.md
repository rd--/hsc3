# hsc3-setup

Co-ordination of [hsc3](http://rd.slavepianos.org/t/hsc3) related packages.

There is a _make_ rule _build-setup_ that builds and installs the
_hsc3-setup_ executable, which depends only on standard haskell
packages (base, directory, filepath, process).

# echo

Print all projects in named category.

Categories are: _core_, _plain_, _ext_, _core+plain_, _all_.

~~~~
hsc3-setup echo all
~~~~

# clone & update

Clone or update packages in _category_ from _source_ repositories to _destination_.

~~~~
hsc3-setup clone core http://rd.slavepianos.org/sw /tmp
hsc3-setup update core http://rd.slavepianos.org/sw ~/sw
~~~~

# local

Run a command at each local directory for _category_.

Common operations are to clean, check for local edits and push any changes to remote.

~~~~
hsc3-setup local all ~/sw cabal clean
hsc3-setup local all ~/sw darcs wh -ls
hsc3-setup local all ~/sw make push-sp
~~~~

# unregsiter & rebuild

~~~~
hsc3-setup unregister all
hsc3-setup rebuild all ~/sw
~~~~

# bootstrap

To setup all _hsc3_ packages on a new machine, given _darcs_ & _ghc_ &
_cabal_, and setting LOCAL appropriately:

~~~~
LOCAL=...
REMOTE=http://rd.slavepianos.org/sw
mkdir -p $LOCAL
cd $LOCAL
darcs get $REMOTE/hsc3-utils/
(cd hsc3-utils ; make build-setup)
hsc3-setup clone all $REMOTE $LOCAL
hsc3-setup unregister all
hsc3-setup rebuild all $LOCAL
~~~~
