
BSCFLAGS=-aggressive-conditions -show-schedule -keep-fires -p +:../paclib

XBSVDIR=../xbsv
BSVDIR=$(XBSVDIR)/bsv
S2H = RbmRequest DmaConfig
H2S = RbmIndication  DmaIndication
BSVFILES = bsv/Rbm.bsv bsv/Top.bsv
CPPFILES= cpp/portalmat.cpp cpp/rbm.cpp cpp/testrbm.cpp
XBSVFLAGS = --clib opencv_core
Dma = Dma32
PINS = Std

include ../xbsv/Makefile.common

datasets:
	mkdir -p datasets
	wget -P datasets http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz
	wget -P datasets http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz
	wget -P datasets http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz
	wget -P datasets http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz
