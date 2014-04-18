#!/bin/sh

OPENCVDIR=$1

cd $OPENCVDIR
mkdir build; cd build

cmake -DCMAKE_TOOLCHAIN_FILE=$OPENCVDIR/platforms/android/android.toolchain.cmake -DANDROID_STL=stlport_static -DANDROID_NATIVE_API_LEVEL=19 ..
make -j8
