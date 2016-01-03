all:
	echo "hsc3"

clean:
	(cd cmd ; make clean)

push-sp:
	darcs push -a rd@slavepianos.org:sw/hsc3

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/hsc3

