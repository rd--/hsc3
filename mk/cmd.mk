prefix = ~/.cabal
cmd_prefix ?= hsc3
cmd_i = $(addprefix $(cmd_prefix)-, $(cmd))

% : %.hs
	ghc -Wall -fwarn-tabs -O2 --make $<

$(cmd_prefix)-% : %
	cp $< $(cmd_prefix)-$<

all: $(cmd)

install: $(cmd_i)
	cp $(cmd_prefix)-* $(prefix)/bin

clean:
	rm -f *.hi *.o
	rm -f $(cmd)
	rm -f $(cmd_i)

query:
	echo prefix = $(prefix)
	echo cmd-prefix = $(cmd_prefix)
	echo cmd = $(cmd)
	echo cmd-i = $(cmd_i)
