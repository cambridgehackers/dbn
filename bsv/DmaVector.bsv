// Copyright (c) 2013 Quanta Research Cambridge, Inc.

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

import GetPut::*;
import FIFOF::*;
import PortalMemory::*;
import Dma::*;
import MemreadEngine::*;
import Adapter::*;
import BRAM::*;
import Pipe::*;

interface VectorSource#(numeric type dsz, type a);
   interface PipeOut#(a) pipe;
   method Action start(DmaPointer h, Bit#(DmaOffsetSize) a, Bit#(DmaOffsetSize) l);
endinterface

interface DmaVectorSource#(numeric type dsz, type a);
   interface DmaReadClient#(dsz) dmaClient;
   interface VectorSource#(dsz, a) vector;
endinterface

function VectorSource#(dsz, dtype) dmaVectorSourceVector(DmaVectorSource#(dsz,dtype) dmavs); return dmavs.vector; endfunction

module [Module] mkDmaVectorSource(DmaVectorSource#(asz, a))
   provisos (  Bits#(a,asz)
	     );

   Bool verbose = False;

   let asz = valueOf(asz);
   let abytes = asz / 8;
   let burstLen = 1;

   FIFOF#(a) dfifo <- mkFIFOF();
   Reg#(DmaPointer) pointer <- mkReg(0);
   Reg#(Bit#(DmaOffsetSize)) offset <- mkReg(0);
   Reg#(Bit#(DmaOffsetSize)) limit <- mkReg(0);

   interface DmaReadClient dmaClient;
      interface Get readReq;
	 method ActionValue#(DmaRequest) get() if (offset < limit);
	    if (verbose) $display("DmaVectorSource.readReq h=%d offset=%h limit=%h len=%d", pointer, offset, limit, burstLen);
	    offset <= offset + fromInteger(abytes)*burstLen;
	    return DmaRequest { pointer: pointer, offset: offset, burstLen: burstLen, tag: 0 };
	 endmethod
      endinterface : readReq
      interface Put readData;
	 method Action put(DmaData#(asz) dmadata);
	    let v  = dmadata.data;
	    if (verbose) $display("DmaVectorSource.readData pointer=%d dmaval=%h", pointer, v);
	    dfifo.enq(unpack(v));
	 endmethod
      endinterface : readData
   endinterface : dmaClient
   interface VectorSource vector;
       method Action start(DmaPointer h, Bit#(DmaOffsetSize) a, Bit#(DmaOffsetSize) l) if (offset >= limit);
	  if (verbose) $display("DmaVectorSource.start h=%d a=%h l=%h", h, a, l);
	  pointer <= h;
	  offset <= a*fromInteger(abytes);
	  limit <= l*fromInteger(abytes);
       endmethod
       interface PipeOut pipe;
	  method first = dfifo.first;
	  method Action deq();
	     if (verbose) $display("DmaReadClient pipe.deq() pointer=%d data=%h", pointer, dfifo.first);
	     dfifo.deq;
	  endmethod
	  method notEmpty = dfifo.notEmpty;
       endinterface
   endinterface
endmodule

interface VectorSink#(numeric type dsz, type a);
   interface PipeOut#(Bool) pipe;
   method Action start(DmaPointer h, Bit#(DmaOffsetSize) a, Bit#(DmaOffsetSize) l);
endinterface
interface DmaVectorSink#(numeric type dsz, type a);
   interface DmaWriteClient#(dsz) dmaClient;
   interface VectorSink#(dsz, a) vector;
endinterface

module [Module] mkDmaVectorSink#(PipeOut#(a) pipe_in)(DmaVectorSink#(asz, a))
   provisos (Bits#(a,asz));

   let asz = valueOf(asz);
   let abytes = asz / 8;
   let burstLen = 1;

   Bool verbose = True;

   FIFOF#(Bool) rfifo <- mkFIFOF();
   Reg#(DmaPointer) pointer <- mkReg(0);
   Reg#(Bit#(DmaOffsetSize)) offset <- mkReg(0);
   Reg#(Bit#(DmaOffsetSize)) doneoffset <- mkReg(0);
   Reg#(Bit#(DmaOffsetSize)) limit <- mkReg(0);

   interface DmaWriteClient dmaClient;
      interface Get writeReq;
	  method ActionValue#(DmaRequest) get if (offset < limit && pipe_in.notEmpty());
	     if (verbose) $display("DmaVectorSink writeReq h=%d offset=%h burstlen=%d pipein.notEmpty=%d rfifo.notFull=%d", pointer, offset, burstLen, pipe_in.notEmpty(), rfifo.notFull());
	     offset <= offset + fromInteger(abytes)*burstLen;
	     return DmaRequest { pointer: pointer, offset: offset, burstLen: burstLen, tag: 0 };
	  endmethod
      endinterface : writeReq
      interface Get writeData;
	 method ActionValue#(DmaData#(asz)) get();
	    let v = pipe_in.first;
	    pipe_in.deq;
	    if (verbose) $display("DmaVectorSink.writeData offset=%h v=%h", offset, v);

	    let da = doneoffset + fromInteger(abytes);
	    $display("DmaVectorSink writeDone da=%d limit=%d", da, limit);
	    if (da >= limit) rfifo.enq(?);
	    doneoffset <= da;

	    return DmaData { data: pack(v), tag: 0 };
	 endmethod
      endinterface : writeData
      interface Put writeDone;
	 method Action put(Bit#(6) tag);
	    // let da = doneoffset + fromInteger(abytes)*burstLen;
	    // $display("DmaVectorSink writeDone tag=%h da=%d limit=%d", tag, da, limit);
	    // if (da >= limit) rfifo.enq(?);
	    // doneoffset <= da;
	 endmethod
      endinterface : writeDone
   endinterface : dmaClient

   interface VectorSink vector;
       method Action start(DmaPointer h, Bit#(DmaOffsetSize) a, Bit#(DmaOffsetSize) l) if (doneoffset >= limit);
	  pointer <= h;
	  offset <= a*fromInteger(abytes);
	  doneoffset <= a*fromInteger(abytes);
	  limit <= l*fromInteger(abytes);
	  if (verbose) $display("DmaVectorSink.start   h=%d offset=%h l=%h", h, a, l);
       endmethod
       interface PipeOut pipe = toPipeOut(rfifo);
   endinterface
endmodule

interface BramVectorSource#(numeric type addrsz, numeric type dsz, type dtype);
   interface BRAMClient#(Bit#(addrsz), dtype) bramClient;
   interface VectorSource#(dsz, dtype) vector;
endinterface

module [Module] mkBramVectorSource(BramVectorSource#(addrsz, dsz, dtype))
   provisos (Bits#(dtype,dsz),
	     Add#(a__, addrsz, DmaOffsetSize)
	     );

   Bool verbose = False;

   let dsz = valueOf(dsz);

   FIFOF#(dtype) dfifo <- mkFIFOF();
   Reg#(Bit#(addrsz)) offset <- mkReg(0);
   Reg#(Bit#(addrsz)) limit <- mkReg(0);

   interface BRAMClient bramClient;
      interface Get request;
	 method ActionValue#(BRAMRequest#(Bit#(addrsz),dtype)) get() if (offset < limit);
	    if (verbose) $display("BramVectorSource.readReq offset=%h limit=%h", offset, limit);
	    offset <= offset + 1;
	    return BRAMRequest { write: False, responseOnWrite: False, address: offset, datain: unpack(0) };
	 endmethod
      endinterface
      interface Put response;
	 method Action put(dtype dmaval);
	    if (verbose) $display("BramVectorSource.readData dmaval=%h", dmaval);
	    dfifo.enq(dmaval);
	 endmethod
      endinterface
   endinterface : bramClient
   interface VectorSource vector;
       method Action start(DmaPointer pointer, Bit#(DmaOffsetSize) a, Bit#(DmaOffsetSize) l) if (offset >= limit);
	  if (verbose) $display("BramVectorSource.start a=%h l=%h", a, l);
	  offset <= truncate(a);
	  limit <= truncate(l);
       endmethod
       interface PipeOut pipe;
	  method first = dfifo.first;
	  method Action deq();
	     if (verbose) $display("BramVectorSource pipe.deq() data=%h", dfifo.first);
	     dfifo.deq;
	  endmethod
	  method notEmpty = dfifo.notEmpty;
       endinterface
   endinterface
endmodule
