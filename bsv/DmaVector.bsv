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
   method Action start(ObjectPointer h, Bit#(ObjectOffsetSize) a, Bit#(ObjectOffsetSize) l);
   method ActionValue#(Bool) finish();
endinterface

interface DmaVectorSource#(numeric type dsz, type a);
   interface ObjectReadClient#(dsz) dmaClient;
   interface VectorSource#(dsz, a) vector;
endinterface

function ObjectReadClient#(asz) getSourceReadClient(DmaVectorSource#(asz,a) s); return s.dmaClient; endfunction
function ObjectWriteClient#(asz) getSinkWriteClient(DmaVectorSink#(asz,a) s); return s.dmaClient; endfunction
function VectorSource#(dsz, dtype) dmaVectorSourceVector(DmaVectorSource#(dsz,dtype) dmavs); return dmavs.vector; endfunction

module [Module] mkDmaVectorSource(DmaVectorSource#(asz, a))
   provisos (Bits#(a,asz),
	     Div#(asz,8,abytes),
	     Log#(abytes,ashift),
	     Mul#(abytes, 8, asz)
	     );

   Bool verbose = True;

   let asz = valueOf(asz);
   let abytes = valueOf(abytes);
   let ashift = valueOf(ashift);
   let burstLen = 1;

   FIFOF#(Bit#(asz)) dfifo <- mkSizedFIFOF(8);
   MemreadEngine#(asz) memreadEngine <- mkMemreadEngine(2, dfifo);

   interface ObjectReadClient dmaClient = memreadEngine.dmaClient;
   interface VectorSource vector;
       method Action start(ObjectPointer p, Bit#(ObjectOffsetSize) a, Bit#(ObjectOffsetSize) l);
	  if (verbose) $display("DmaVectorSource.start h=%d a=%h l=%h ashift=%d", p, a, l, ashift);
          memreadEngine.start(p, a << ashift, truncate(l << ashift), 1 << ashift);
       endmethod
      method ActionValue#(Bool) finish();
	 let b <- memreadEngine.finish();
	 return b;
      endmethod
       interface PipeOut pipe;
	  method first();
	     return unpack(dfifo.first());
	  endmethod
	  method Action deq();
	     if (verbose) $display("ObjectReadClient pipe.deq() data=%h", dfifo.first);
	     dfifo.deq;
	  endmethod
	  method notEmpty = dfifo.notEmpty;
       endinterface
   endinterface
endmodule

interface VectorSink#(numeric type dsz, type a);
   interface PipeOut#(Bool) pipe;
   method Action start(ObjectPointer h, Bit#(ObjectOffsetSize) a, Bit#(ObjectOffsetSize) l);
endinterface
interface DmaVectorSink#(numeric type dsz, type a);
   interface ObjectWriteClient#(dsz) dmaClient;
   interface VectorSink#(dsz, a) vector;
endinterface

module [Module] mkDmaVectorSink#(PipeOut#(a) pipe_in)(DmaVectorSink#(asz, a))
   provisos (Bits#(a,asz));

   let asz = valueOf(asz);
   let abytes = asz / 8;
   let burstLen = 1;

   Bool verbose = False;

   FIFOF#(Bool) rfifo <- mkFIFOF();
   Reg#(ObjectPointer) pointer <- mkReg(0);
   Reg#(Bit#(ObjectOffsetSize)) offset <- mkReg(0);
   Reg#(Bit#(ObjectOffsetSize)) doneoffset <- mkReg(0);
   Reg#(Bit#(ObjectOffsetSize)) limit <- mkReg(0);

   interface ObjectWriteClient dmaClient;
      interface Get writeReq;
	  method ActionValue#(ObjectRequest) get if (offset < limit && pipe_in.notEmpty());
	     if (verbose) $display("DmaVectorSink writeReq h=%d offset=%h burstlen=%d pipein.notEmpty=%d rfifo.notFull=%d", pointer, offset, burstLen, pipe_in.notEmpty(), rfifo.notFull());
	     offset <= offset + fromInteger(abytes)*burstLen;
	     return ObjectRequest { pointer: pointer, offset: offset, burstLen: burstLen, tag: 0 };
	  endmethod
      endinterface : writeReq
      interface Get writeData;
	 method ActionValue#(ObjectData#(asz)) get();
	    let v = pipe_in.first;
	    pipe_in.deq;
	    if (verbose) $display("DmaVectorSink.writeBack offset=%h v=%h", offset, v);
	    return ObjectData { data: pack(v), tag: 0 };
	 endmethod
      endinterface : writeData
      interface Put writeDone;
	 method Action put(Bit#(6) tag);
	    //$display("DmaVectorSink writeDone tag=%h", tag);
	    let da = doneoffset + fromInteger(abytes)*burstLen;
	    if (da >= limit)
	       rfifo.enq(?);
	    doneoffset <= da;
	 endmethod
      endinterface : writeDone
   endinterface : dmaClient

   interface VectorSink vector;
       method Action start(ObjectPointer h, Bit#(ObjectOffsetSize) a, Bit#(ObjectOffsetSize) l) if (doneoffset >= limit);
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
	     Add#(a__, addrsz, ObjectOffsetSize)
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
       method Action start(ObjectPointer pointer, Bit#(ObjectOffsetSize) a, Bit#(ObjectOffsetSize) l) if (offset >= limit);
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
