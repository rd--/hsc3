all:
	ghc --make -Wall Sound.SC3

clean:
	rm -f *.aux *.log *.pdf *.tex */*.o */*.hi */*/*.o */*/*.hi */*/*/*.o */*/*/*.hi 
