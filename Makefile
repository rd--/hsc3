all:
	echo "hsc3"

clean:
	(cd cmd ; make clean)
	rm -Rf dist

push-rd:
	darcs push -a rd@rohandrape.net:sw/hsc3

pull-rd:
	darcs pull -a http://rohandrape.net/sw/hsc3

