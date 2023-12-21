all:
	echo "hsc3"

clean:
	rm -Rf dist dist-newstyle *~

push-all:
	r.gitlab-push.sh hsc3
	r.github-push.sh hsc3

push-tags:
	r.gitlab-push.sh hsc3 --tags
	r.github-push.sh hsc3 --tags

indent:
	fourmolu -i Sound

doctest:
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns Sound
