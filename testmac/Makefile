BSCFLAGS=-aggressive-conditions -show-schedule -keep-fires -p +:../paclib

XBSVDIR=../../xbsv
DBNDIR=..

BSVDIR=$(XBSVDIR)/bsv
S2H =  FpMacRequest
H2S =  FpMacIndication
BSVFILES = $(DBNDIR)/bsv/RbmTypes.bsv  $(DBNDIR)/bsv/FpMacTb.bsv Top.bsv
CPPFILES=  testmac.cpp
XBSVFLAGS = --clib opencv_core
PINS = Std

include $(XBSVDIR)/Makefile.common
