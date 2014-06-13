
BSCFLAGS=-aggressive-conditions -show-schedule -keep-fires -p +:../paclib

XBSVDIR=../../xbsv
DBNDIR=..
DBNTOPBSV=../testmm/Top.bsv
TESTCPPFILES=../testmm/testmm.cpp
XBSVFLAGS = -l opencv_core
XBSVFLAGS += -D NumberOfMasters=1
XBSVFLAGS += -D J_VALUE=1 -D K_VALUE=1 -D N_VALUE=2
include ../Makefile.dbn
