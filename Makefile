GH=https://github.com/rd--/hsc3
GL=https://gitlab.com/rd--/hsc3

all:
	echo "hsc3"

clean:
	(cd cmd ; make clean)
	rm -Rf dist

push-gl:
	git push $(GL)

pull-gl:
	git pull $(GL)

update-rd:
	ssh rd@rohandrape.net "(cd sw/hsc3;git pull $(GL))"

push-gh:
	git push $(GH)

pull-gh:
	git pull $(GH)

