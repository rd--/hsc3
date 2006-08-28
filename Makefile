flags = -W -O2 -fexcess-precision

all:
	ghc --make $(flags) Hsc

clean:
	rm -f *.aux *.log *.pdf *.tex *.o *.hi */*.o */*.hi */*/*.o */*/*.hi
