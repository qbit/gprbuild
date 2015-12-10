
export MARKER=__0000000011111111112222222222333333333444444444455555555556666666666777777777778888888889999999__
export TARGET=${MAKELEVEL}_${MARKER}_${MAKELEVEL}
export BASE=${CURDIR}

export DEPTH=20
export ADA_COUNT=10
export C_COUNT=100
export PROJECT_COUNT=10


all:
	gprbuild -p -P prj
	rm -rf tgt
	mkdir -p tgt
	cp Makefile.wrk tgt/
	@${MAKE} -C tgt  ${@} -f Makefile.wrk
clean:
	rm -rf tgt .obj
	rm -f *.db *~ *.xml *.o *.ali make_sources

