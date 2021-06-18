cmd = scsynth setup

include ~/sw/hsc3/mk/cmd.mk

build-setup:
	make setup
	mkdir -p $(prefix)/bin
	cp setup $(prefix)/bin/hsc3-setup

dep:
	cabal v1-install data-ordlist split regex-compat

print-exec:
	hsc3-setup cabal print-exec hsc3- [a-z]*.hs
