R=https://github.com/rd--/hsc3

all:
	echo "hsc3"

clean:
	(cd cmd ; make clean)
	rm -Rf dist

push-gh:
	git push $(R)

pull-gh:
	git pull $(R)

update-rd:
	ssh rd@rohandrape.net "(cd sw/hsc3;git pull $(R))"
