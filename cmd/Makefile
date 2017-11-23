cmd = scsyndef-stat scsyndef-to-hs scsynth setup

include ~/sw/hsc3/mk/cmd.mk

build-setup:
	make setup
	make install hsc3-setup

print-exec:
	hsc3-setup cabal print-exec hsc3- [a-z]*.hs
