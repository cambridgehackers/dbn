
BSCFLAGS=-aggressive-conditions -show-schedule -keep-fires -p +:../paclib

XBSVDIR=../xbsv
DBNDIR=.
DBNTOPBSV=testrbm/Top.bsv
TESTCPPFILES=cpp/testrbm.cpp
include Makefile.dbn
