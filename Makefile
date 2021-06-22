GL_GIT=git@gitlab.com:rd--/hsc3.git
GL_HTTP=https://gitlab.com/rd--/hsc3.git

GH_GIT=git@github.com:rd--/hsc3.git
GH_HTTP=https://github.com/rd--/hsc3.git

all:
	echo "hsc3"

clean:
	rm -Rf dist dist-newstyle

mk-tags:
	find . -name '*.hs' | xargs hasktags -e

push-gl:
	git push $(GL_GIT)

pull-gl:
	git pull $(GL_HTTP)

push-gh:
	git push $(GH_GIT)

pull-gh:
	git pull $(GH_HTTP)

push-tags:
	git push $(GL_GIT) --tags
	git push $(GH_GIT) --tags

update-rd:
	ssh rd@rohandrape.net "(cd sw/hsc3;git pull $(GL_HTTP))"

push-all:
	make push-gl push-gh update-rd
