all:
	ghc --make -W -O2 Hsc

clean:
	rm -f *.aux *.log *.pdf *.tex *.o *.hi */*.o */*.hi

