CMAKE_MINIMUM_REQUIRED( VERSION 2.8 )

#########################################################
# Set project name
#########################################################

SET( PROJECT_NAME android_exe )

PROJECT( ${PROJECT_NAME} )

#########################################################
# Find OpenCV
#########################################################

FIND_PACKAGE( OpenCV REQUIRED )

#########################################################
# c/c++ flags, includes and lib dependencies
#########################################################

#notice the "recycling" of CMAKE_C_FLAGS
#this is necessary to pick up android flags
SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -pedantic -DZYNQ -DMMAP_HW" )
SET( CMAKE_CPP_FLAGS "${CMAKE_CPP_FLAGS} -Wall -pedantic -DZYNQ -DMMAP_HW" )
SET( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DZYNQ -DMMAP_HW" )

SET(XBSVDIR /scratch/jamey/xbsv)
SET(JNIDIR  ${CMAKE_CURRENT_SOURCE_DIR}/zc702/jni )
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/cpp ${JNIDIR} ${XBSVDIR}/cpp ${XBSVDIR} ${XBSVDIR}/drivers/zynqportal)

SET( LIBRARY_DEPS ${OpenCV_LIBS} )
IF( ANDROID )
  SET( LIBRARY_DEPS ${LIBRARY_DEPS} log dl )
ENDIF()

#########################################################
# source files
#########################################################

FILE( GLOB hdrs "cpp/*.h*" )
FILE( GLOB srcs "cpp/*.cpp" "${JNIDIR}/*.cpp" ${XBSVDIR}/cpp/portal.cpp ${XBSVDIR}/cpp/PortalMemory.cpp)

ADD_EXECUTABLE( ${PROJECT_NAME} ${srcs} )
TARGET_LINK_LIBRARIES( ${PROJECT_NAME} ${LIBRARY_DEPS} /scratch/android-ndk-r9d/sources/cxx-stl/stlport/libs/armeabi-v7a/libstlport_static.a)

#########################################################
# Summary report
#########################################################
message( STATUS "")
message( STATUS "General configuration for ${PROJECT_NAME} =====================================")
message( STATUS "")
message( STATUS "    OpenCV path:                ${OpenCV_DIR}")
message( STATUS "    Compiler:                   ${CMAKE_CXX_COMPILER}")
message( STATUS "    C++ flags (Release):        ${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_RELEASE}")
message( STATUS "    C++ flags (Debug):          ${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_DEBUG}")
if(WIN32)
message( STATUS "    Linker flags (Release):     ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_EXE_LINKER_FLAGS_RELEASE}")
message( STATUS "    Linker flags (Debug):       ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_EXE_LINKER_FLAGS_DEBUG}")
else()
message( STATUS "    Linker flags (Release):     ${CMAKE_SHARED_LINKER_FLAGS} ${CMAKE_SHARED_LINKER_FLAGS_RELEASE}")
message( STATUS "    Linker flags (Debug):       ${CMAKE_SHARED_LINKER_FLAGS} ${CMAKE_SHARED_LINKER_FLAGS_DEBUG}")
endif()
