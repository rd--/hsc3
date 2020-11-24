GH=https://github.com/rd--/hsc3
GL=gitlab.com:rd--/hsc3.git

all:
	echo "hsc3"

clean:
	(cd cmd ; make clean)
	rm -Rf dist

mk-cmd:
	(cd cmd ; make all install)

push-gl:
	git push git@$(GL)

pull-gl:
	git pull https://$(GL)

update-rd:
	ssh rd@rohandrape.net "(cd sw/hsc3;git pull https://$(GL))"

push-rd:
	make push-gl update-rd

push-gh:
	git push $(GH)

pull-gh:
	git pull $(GH)

