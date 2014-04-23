
// Copyright (c) 2014 Quanta Research Cambridge, Inc.

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <stdio.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <semaphore.h>
#include <pthread.h>
#include <errno.h>
#include <math.h>
#include <assert.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include <GeneratedTypes.h>
#include <FpMacRequestProxy.h>
#include <FpMacIndicationWrapper.h>

class FpMacIndication : public FpMacIndicationWrapper
{
public:
  virtual void res(uint32_t v, uint32_t e) {
    fprintf(stderr, "res: %d %d\n", v, e);
	exit(0);
    }
    FpMacIndication(unsigned int id) : FpMacIndicationWrapper(id) {}
};


int main(int argc, const char **argv)
{
  unsigned int srcGen = 0;

  fprintf(stderr, "%s %s\n", __DATE__, __TIME__);
  FpMacRequestProxy *dev = new FpMacRequestProxy(IfcNames_FpMacRequestPortal);
  FpMacIndication   *ind = new FpMacIndication(IfcNames_FpMacIndicationPortal);

  pthread_t tid;
  fprintf(stderr, "creating exec thread\n");
  if(pthread_create(&tid, NULL,  portalExec, NULL)){
   fprintf(stderr, "error creating exec thread\n");
   exit(1);
  }

  dev->mac(0,0);
  while(1);
}
