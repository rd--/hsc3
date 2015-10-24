prefix = ~/.cabal
cmd_i = $(addprefix hsc3-, $(cmd))

% : %.hs
	ghc -Wall -fwarn-tabs -O2 --make $<

hsc3-% : %
	cp $< hsc3-$<

all: $(cmd)

install: $(cmd_i)
	cp hsc3-* $(prefix)/bin

clean:
	rm -f *.hi *.o
	rm -f $(cmd)
	rm -f $(cmd_i)
