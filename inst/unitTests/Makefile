TOP=../../..
RUNIT_DIR=$(shell pwd)

all: test

testonly:
	R --slave < runalltests.R

test:
	cd ${TOP}; \
	R CMD INSTALL hypergraph && \
	cd ${RUNIT_DIR}
	R --slave < runalltests.R

