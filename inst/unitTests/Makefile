TOP=../..
RUNIT_DIR=$(shell pwd)

all:
	echo ${TOP}
	echo ${RUNIT_DIR}

test:
	cd ${TOP}; \
	R CMD INSTALL hypergraph; \
	cd ${RUNIT_DIR}
	R --slave < runalltests.R

