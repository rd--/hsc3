all:
	runhaskell Setup.lhs configure --user --prefix ~
	runhaskell Setup.lhs build
	runhaskell Setup.lhs install --user

clean:
	runhaskell Setup.lhs clean
