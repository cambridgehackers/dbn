#!/bin/sh
cd `dirname $0`

BUILD_DIR=build_armeabi
opencv_android=/scratch/jamey/opencv/platforms/android
opencv_build_dir=/scratch/jamey/opencv/build

mkdir -p $BUILD_DIR
cd $BUILD_DIR

RUN_CMAKE="cmake -DOpenCV_DIR=$opencv_build_dir -DOpenCV_MODULES_SUFFIX=_armeabi_v7a -DANDROID_NDK_ABI_NAME=armeabi-v7a -DANDROID_NATIVE_API_LEVEL=19 -DCMAKE_TOOLCHAIN_FILE=$opencv_android/android.toolchain.cmake .."
echo $RUN_CMAKE
$RUN_CMAKE
make
