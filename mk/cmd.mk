prefix = ~/.cabal
cmd_prefix ?= hsc3
cmd_rw = $(addprefix $(cmd_prefix)-, $(cmd))

% : %.hs
	ghc -Wall -fwarn-tabs -O2 --make $<

$(cmd_prefix)-% : %
	cp $< $(cmd_prefix)-$<

all: $(cmd)

install: $(cmd_rw)
	cp $(cmd_rw) $(prefix)/bin

clean:
	rm -f *.hi *.o
	rm -f $(cmd)
	rm -f $(cmd_rw)

query:
	echo prefix = $(prefix)
	echo cmd-prefix = $(cmd_prefix)
	echo cmd = $(cmd)
	echo cmd-rw = $(cmd_rw)
