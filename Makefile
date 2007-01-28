all:
	runhaskell Setup.hs configure --prefix ~
	runhaskell Setup.hs build
	runhaskell Setup.hs install --user

clean:
	runhaskell Setup.hs clean
