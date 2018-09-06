R=https://github.com/rd--/hsc3

all:
	echo "hsc3"

clean:
	(cd cmd ; make clean)
	rm -Rf dist

push-gh:
	darcs push -a $(R)

pull-rd:
	darcs pull -a $(R)

update-rd:
	ssh rd@rohandrape.net "(cd sw/hsc3;git pull $(R))"
