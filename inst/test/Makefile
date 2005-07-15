TOP=../../..
RUNIT_DIR=$(shell pwd)

all:
	echo ${TOP}
	echo ${RUNIT_DIR}

test:
	cd ${TOP}; \
	R CMD INSTALL SharedItems; \
	cd ${RUNIT_DIR}
	R --slave < testall.R
