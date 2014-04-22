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

import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import Vector::*;
import BRAM::*;
import DmaVector::*;
import PortalMemory::*;
import Dma::*;
import FloatingPoint::*;
import Pipe::*;
import FloatOps::*;
import Timer::*;
import RbmTypes::*;

module [Module] mkDotProd#(PipeOut#(Vector#(n,Float)) xpipe, PipeOut#(Vector#(n,Float)) ypipe, UInt#(nwidth) numElts, UInt#(TLog#(n)) label)(PipeOut#(Float))
   provisos (
      Add#(1,a__,n),
      ReducePipe#(n,Float)
      );

   Bool verbose = False;
   Bool useMac = True;

   Reg#(UInt#(nwidth)) countInReg <- mkReg(0);
   FIFOF#(Tuple3#(Bool,Vector#(n,Float),Vector#(n,Float))) xyfifo <- mkFIFOF();
   FIFOF#(Bool) firstFifo <- mkFIFOF();
   FIFOF#(Bool) lastFifo <- mkFIFOF();
   PipeOut#(Tuple3#(Bool, Vector#(n,Float),Vector#(n,Float))) xypipe = toPipeOut(xyfifo);
   rule xyin;
      Bool isFirst = (countInReg == 0);
      UInt#(nwidth) n = fromInteger(valueOf(n));
      let c = countInReg+n;
      Bool isLast = (c >= numElts);
      if (isLast)
	 c = 0;
      countInReg <= c;
      xpipe.deq();
      ypipe.deq();
      xyfifo.enq(tuple3(isFirst,xpipe.first(), ypipe.first()));
      if (!useMac)
	 firstFifo.enq(isFirst);
      lastFifo.enq(isLast);
   endrule


   Vector#(n, Server#(Tuple4#(Maybe#(Float), Float, Float, RoundMode), Tuple2#(Float,Exception))) macs <- replicateM(mkFloatMac);
   Vector#(n, FloatServer2#(Float)) muls <- replicateM(mkFloatMultiplier);
   Vector#(n, FloatServer2#(Float)) adders <- replicateM(mkFloatAdder);
   Vector#(n, FIFO#(Float)) accumFifo <- replicateM(mkFIFO());
   FIFOF#(Vector#(n,Float)) resultFifo <- mkFIFOF();

   rule mul;
      let v = xypipe.first;
      xypipe.deq;
      let isFirst = tpl_1(v);
      let x = tpl_2(v);
      let y = tpl_3(v);

      for (Integer i = 0; i < valueOf(n); i = i + 1) begin
	 Maybe#(Float) accum = tagged Invalid;
	 if (useMac) begin
	     if (!isFirst) begin
		accum = tagged Valid accumFifo[i].first();
		accumFifo[i].deq();
	     end
            macs[i].request.put(tuple4(accum, x[i], y[i], defaultValue));
	 end
	 else begin
	    muls[i].request.put(tuple3(x[i],y[i],defaultValue));
	 end
	 if (verbose) $display($format(fshow("label=")+fshow(label)+fshow(" dotprod x=") + fshow(x) + fshow(" y=") + fshow(y) + fshow(" accum=")+fshow(pack(accum))));
      end
   endrule

   rule add;
      if (!useMac) begin
	 Float accum = unpack(0);
	 let isFirst = firstFifo.first();
	 firstFifo.deq();
	 for (Integer i = 0; i < valueOf(n); i = i + 1) begin
	    if (!isFirst) begin
	       accum = accumFifo[i].first();
	       accumFifo[i].deq();
	    end
	    let resp <- adders[i].response.get();
	    adders[i].request.put(tuple3(accum,tpl_1(resp),defaultValue));	 
	 end
      end
   endrule

   rule macout;
      Vector#(n,Float) vs = unpack(0);
      let isLast = lastFifo.first();
      lastFifo.deq();
      for (Integer i = 0; i < valueOf(n); i = i + 1) begin
	 Float vi;
	 if (useMac) begin
	    let resp <- macs[i].response.get();
	    vi = tpl_1(resp);
	 end
	 else begin
	    let resp <- adders[i].response.get();
	    vi = tpl_1(resp);
	 end
	 vs[i] = vi;
	 if (!isLast) begin
	    accumFifo[i].enq(vi);
	 end
      end
      if (isLast) begin
	 Vector#(n,Bit#(32)) ivs = unpack(pack(vs));
	 resultFifo.enq(vs);
	 if (verbose) $display(fshow("label=")+fshow(label)+fshow(" isLast=")+fshow(isLast)+fshow(" numElts=")+fshow(numElts)+fshow(" mul=") + fshow(ivs));
      end
   endrule

   PipeOut#(Vector#(n,Float)) resultPipe = toPipeOut(resultFifo);
   PipeOut#(Float) dotpipe <- mkReducePipe(mkFloatAddPipe, resultPipe);
   PipeOut#(Float) displaydotpipe = (interface PipeOut#(Float);
      method Float first(); return dotpipe.first(); endmethod
      method Action deq(); $display(fshow("label=")+fshow(label)+fshow(" dotpipe=")+fshow(dotpipe.first())); dotpipe.deq(); endmethod
      method Bool notEmpty(); return dotpipe.notEmpty(); endmethod
      endinterface);
   return dotpipe;
endmodule

interface DotProdServer#(numeric type n);
   interface Reg#(UInt#(20)) numElts;
   interface Put#(Tuple2#(Vector#(n,Float),Vector#(n,Float))) request;
   interface PipeOut#(Float) pipe;
endinterface

(* synthesize *)
module [Module] mkDotProdServer#(UInt#(TLog#(N)) label)(DotProdServer#(N));

   let n = valueOf(N);
   Bool verbose = False;
   Bool useMac = True;

   Reg#(UInt#(20)) numEltsReg <- mkReg(0);
   Reg#(UInt#(20)) countInReg <- mkReg(0);
   FIFOF#(Tuple3#(Bool,Vector#(N,Float),Vector#(N,Float))) xyfifo <- mkFIFOF();
   FIFOF#(Bool) firstFifo <- mkFIFOF();
   FIFOF#(Bool) lastFifo <- mkFIFOF();
   PipeOut#(Tuple3#(Bool, Vector#(N,Float),Vector#(N,Float))) xypipe = toPipeOut(xyfifo);

   Vector#(N, Server#(Tuple4#(Maybe#(Float), Float, Float, RoundMode), Tuple2#(Float,Exception))) macs <- replicateM(mkFloatMac);
   Vector#(N, FloatServer2#(Float)) muls <- replicateM(mkFloatMultiplier);
   Vector#(N, FloatServer2#(Float)) adders <- replicateM(mkFloatAdder);
   Vector#(N, FIFO#(Float)) accumFifo <- replicateM(mkFIFO());
   FIFOF#(Vector#(N,Float)) resultFifo <- mkFIFOF();

   rule mul;
      let v = xypipe.first;
      xypipe.deq;
      let isFirst = tpl_1(v);
      let x = tpl_2(v);
      let y = tpl_3(v);

      for (Integer i = 0; i < valueOf(N); i = i + 1) begin
	 Maybe#(Float) accum = tagged Invalid;
	 if (useMac) begin
	     if (!isFirst) begin
		accum = tagged Valid accumFifo[i].first();
		accumFifo[i].deq();
	     end
            macs[i].request.put(tuple4(accum, x[i], y[i], defaultValue));
	 end
	 else begin
	    muls[i].request.put(tuple3(x[i],y[i],defaultValue));
	 end
	 if (verbose) $display($format(fshow("label=")+fshow(label)+fshow(" dotprod x=") + fshow(x) + fshow(" y=") + fshow(y) + fshow(" accum=")+fshow(pack(accum))));
      end
   endrule

   rule add;
      if (!useMac) begin
	 Float accum = unpack(0);
	 let isFirst = firstFifo.first();
	 firstFifo.deq();
	 for (Integer i = 0; i < valueOf(N); i = i + 1) begin
	    if (!isFirst) begin
	       accum = accumFifo[i].first();
	       accumFifo[i].deq();
	    end
	    let resp <- adders[i].response.get();
	    adders[i].request.put(tuple3(accum,tpl_1(resp),defaultValue));	 
	 end
      end
   endrule

   rule macout;
      Vector#(N,Float) vs = unpack(0);
      let isLast = lastFifo.first();
      lastFifo.deq();
      for (Integer i = 0; i < valueOf(N); i = i + 1) begin
	 Float vi;
	 if (useMac) begin
	    let resp <- macs[i].response.get();
	    vi = tpl_1(resp);
	 end
	 else begin
	    let resp <- adders[i].response.get();
	    vi = tpl_1(resp);
	 end
	 vs[i] = vi;
	 if (!isLast) begin
	    accumFifo[i].enq(vi);
	 end
      end
      if (isLast) begin
	 Vector#(N,Bit#(32)) ivs = unpack(pack(vs));
	 resultFifo.enq(vs);
	 if (verbose) $display(fshow("label=")+fshow(label)+fshow(" isLast=")+fshow(isLast)+fshow(" numElts=")+fshow(numEltsReg)+fshow(" mul=") + fshow(ivs));
      end
   endrule

   PipeOut#(Vector#(N,Float)) resultPipe = toPipeOut(resultFifo);
   PipeOut#(Float) dotpipe <- mkReducePipe(mkFloatAddPipe, resultPipe);
   PipeOut#(Float) displaydotpipe = (interface PipeOut#(Float);
      method Float first(); return dotpipe.first(); endmethod
      method Action deq(); $display(fshow("label=")+fshow(label)+fshow(" dotpipe=")+fshow(dotpipe.first())); dotpipe.deq(); endmethod
      method Bool notEmpty(); return dotpipe.notEmpty(); endmethod
      endinterface);
   interface Put request;
      method Action put(Tuple2#(Vector#(N,Float),Vector#(N,Float)) tpl);
	 Bool isFirst = (countInReg == 0);
	 UInt#(20) n = fromInteger(valueOf(N));
	 let c = countInReg+n;
	 Bool isLast = (c >= numEltsReg);
	 if (isLast)
	    c = 0;
	 countInReg <= c;
	 xyfifo.enq(tuple3(isFirst,tpl_1(tpl), tpl_2(tpl)));
	 if (!useMac)
	    firstFifo.enq(isFirst);
	 lastFifo.enq(isLast);
      endmethod
   endinterface : request
   interface PipeOut pipe = dotpipe;
   interface Reg numElts = numEltsReg;
endmodule


typedef struct {
   a xbase;
   a xlimit;
   a xstep;
   a ybase;
   a ylimit;
   a ystep;
} XYRangeConfig#(type a) deriving (Bits, FShow);

interface XYRangePipeIfc#(type a);
   interface PipeOut#(Tuple2#(a,a)) pipe;
   method Action start(XYRangeConfig#(a) cfg);
   method Action display();
endinterface

module mkXYRangePipeOut(XYRangePipeIfc#(a)) provisos (Arith#(a), Bits#(a,awidth), Eq#(a), Ord#(a));
   Reg#(a) x <- mkReg(0);
   Reg#(a) y <- mkReg(0);
   Reg#(a) xbase <- mkReg(0);
   Reg#(a) ybase <- mkReg(0);
   Reg#(a) xstep <- mkReg(0);
   Reg#(a) ystep <- mkReg(0);
   Reg#(a) xlimit <- mkReg(0);
   Reg#(a) ylimit <- mkReg(0);

   interface PipeOut pipe;
      method Tuple2#(a,a) first() if (x < xlimit && y < ylimit);
	 return tuple2(x,y);
      endmethod
      method Action deq if (x < xlimit && y < ylimit);
	 let newx = x;
	 let newy = y+ystep;
	 if (newy >= ylimit && x < xlimit) begin
	    newy = ybase;
	    newx = newx + xstep;
	 end
	 x <= newx;
	 y <= newy;
      endmethod
      method Bool notEmpty();
	 return (x < xlimit && y < ylimit);
      endmethod
   endinterface
   method Action start(XYRangeConfig#(a) cfg) if (x >= xlimit);
      //$display("XYRangePipe x=%d xlimit=%d xstep=%d y=%d ylimit=%d ystep=%d", cfg.xbase, cfg.xlimit, cfg.xstep, cfg.ybase, cfg.ylimit, cfg.ystep);
      x <= cfg.xbase;
      y <= cfg.ybase;
      xbase <= cfg.xbase;
      ybase <= cfg.ybase;
      xstep <= cfg.xstep;
      ystep <= cfg.ystep;
      xlimit <= cfg.xlimit;
      ylimit <= cfg.ylimit;
   endmethod
   method Action display();
      $display("XYRangePipe x=%d xlimit=%d y=%d ylimit=%d xstep=%d ystep=%d", x, xlimit, xstep, y, ylimit, ystep);
   endmethod
endmodule: mkXYRangePipeOut

typedef struct {
   ObjectPointer pointer;
   addrtype base;
   addrtype numRows;
   addrtype numColumns;
} MatrixDescriptor#(type addrtype) deriving (Bits);

// row major layout
interface DmaMatrixMultiplyIfc#(numeric type addrwidth, numeric type dsz);
   interface ObjectWriteClient#(dsz) dmaClient;
   method Action start(ObjectPointer pointerA, UInt#(addrwidth) numRowsA, UInt#(addrwidth) numColumnsA,
		       ObjectPointer pointerB, UInt#(addrwidth) numRowsB, UInt#(addrwidth) numColumnsB,
		       ObjectPointer pointerC);
   method ActionValue#(Bool) finish();
endinterface

typedef enum {
   Idle, Ready, Running, Done
   } MMState deriving (Bits, Eq);

module [Module] mkDmaMatrixMultiply#(Vector#(1, VectorSource#(dsz, Vector#(N, Float))) sourceA,
				     Vector#(N, VectorSource#(dsz, Vector#(N, Float))) sourceB,
				     function Module#(DmaVectorSink#(dsz, Vector#(N, Float))) mkSink(PipeOut#(Vector#(N, Float)) pipe_in)
				     )(DmaMatrixMultiplyIfc#(addrwidth, dsz))
   provisos (Add#(a__,ObjectOffsetSize,addrwidth)
	     , Add#(N,0,n)
	     , Log#(N,nshift)
	     , FShow#(Float)
	     , Arith#(Float)
	     , Bits#(Vector#(N, Float), dsz)
	     , Bits#(MatrixDescriptor#(UInt#(addrwidth)), mdsz)
	     , Bits#(Tuple2#(UInt#(addrwidth), UInt#(addrwidth)), tplsz)
	     , Add#(b__, 20, addrwidth)
      );

   let n = valueOf(n);
   let nshift = valueOf(nshift);
   Bool verbose = False;

   Reg#(Bool) doneReg <- mkReg(False);
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorA <- mkReg(unpack(0));
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorB <- mkReg(unpack(0));
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorC <- mkReg(unpack(0));
   Reg#(UInt#(addrwidth)) dotprodCount <- mkReg(0);

   Vector#(n, PipeOut#(Vector#(N,Float))) aPipes <- mkForkVector(sourceA[0].pipe);

   function Module#(DotProdServer#(N)) mkFxDotProd(Integer i);
      return mkDotProdServer(fromInteger(i));
   endfunction
   Vector#(n, DotProdServer#(N)) fxdotprods <- genWithM(mkFxDotProd);
   for (Integer i = 0; i < n; i = i+1)
      rule connectDotProd;
	 let a = aPipes[i].first();
	 aPipes[i].deq();
	 let b = sourceB[i].pipe.first();
	 sourceB[i].pipe.deq();
	 fxdotprods[i].request.put(tuple2(a, b));
      endrule

   FIFOF#(Vector#(N,Float)) dfifo <- mkFIFOF();
   let sinkC <- mkSink(toPipeOut(dfifo));

   XYRangePipeIfc#(UInt#(addrwidth)) xypipeifc <- mkXYRangePipeOut();

   Vector#(TAdd#(n,2), PipeOut#(Tuple2#(UInt#(addrwidth),UInt#(addrwidth)))) xypipes <- mkForkVector(xypipeifc.pipe);

   Reg#(Bool) running <- mkReg(False);
   FIFOF#(Bool) doneFifo <- mkFIFOF();

   Reg#(UInt#(32)) cycles <- mkReg(0);
   rule countCycles;
      cycles <= cycles+1;
   endrule

   for (Integer i = 0; i < n; i = i + 1) begin
      rule startDotProd;
	  Tuple2#(UInt#(addrwidth),UInt#(addrwidth)) xy = xypipes[i].first;
	  xypipes[i].deq();

	  let row = tpl_1(xy);
	  let col = tpl_2(xy)+fromInteger(i);

	  let startA = row*descriptorA.numColumns; // row major
	  let startB = col*descriptorB.numColumns; // col major layout (pre-transposed)
	  let startC = row*descriptorC.numColumns + col; // row major
	 if (verbose) $display($format(fshow(cycles)+fshow("    startDotProd xy=")+fshow(tuple2(row,col))
				       +fshow(" startA=")+fshow(startA)
				       +fshow(" startB=")+fshow(startB)
				       +fshow(" startC=")+fshow(startC)));

	  if (i == 0) begin
	     sourceA[0].start(descriptorA.pointer, pack(truncate(startA>>nshift)), pack(truncate(descriptorA.numColumns>>nshift)));
	     if (verbose) $display($format(fshow(cycles)+fshow("    sourceA[0].start")+fshow(startA)));
	  end
	 sourceB[i].start(descriptorB.pointer, pack(truncate(startB>>nshift)), pack(truncate(descriptorB.numColumns>>nshift)));
	 UInt#(TLog#(n)) in = fromInteger(i);
	 Bit#(TAdd#(1,TLog#(n))) nn = fromInteger(n);
	 if (verbose) $display($format(fshow(cycles)+fshow("    sourceB[")+fshow(in)+fshow("].start")+fshow(startB)));
	  if (i == 0)
	     sinkC.vector.start(descriptorC.pointer, pack(truncate(startC>>nshift)), 1);
       endrule
       if (i == 0)
	  rule finishSourceA;
	     if (verbose) $display($format(fshow(cycles)+fshow("    sourceA[0].finish ")));
	     let b <- sourceA[0].finish();
	  endrule
      rule finishSourceB;
	 UInt#(TLog#(n)) in = fromInteger(i);
	 if (verbose) $display($format(fshow(cycles)+fshow("    sourceB[")+fshow(in)+fshow("].finish")));
	 let b <- sourceB[i].finish();
      endrule
      
   end

   rule dotProdValue;
      Tuple2#(UInt#(addrwidth),UInt#(addrwidth)) xy = xypipes[n].first;
      xypipes[n].deq;
      Vector#(N,Float) vs;
      for (Integer i = 0; i < n; i = i + 1) begin
	 let v = fxdotprods[i].pipe.first;
	 fxdotprods[i].pipe.deq;
	 let xyi = tuple2(tpl_1(xy), tpl_2(xy)+fromInteger(i));
	 if (verbose) $display($format(fshow(cycles)+fshow("    dotprodvalue xy=")+fshow(xyi)+fshow(" dotprod=")+fshow(v)));
	 vs[i] = v;
      end
      dfifo.enq(vs);
   endrule

   rule sinkDone;
      // each time we write a burst of k values via sinkC
      let xy = xypipes[n+1].first;
      xypipes[n+1].deq;
      let b <- sinkC.vector.finish();
      let c = dotprodCount-fromInteger(n);
      if (verbose) $display($format(fshow(cycles)+fshow("    sinkDone c")+fshow(c)+fshow("    xy=")+fshow(xy)));
      dotprodCount <= c;
      if (c == 0) begin
	 running <= False;
	 doneFifo.enq(?);
      end
   endrule

   method Action start(ObjectPointer pointerA, UInt#(addrwidth) numRowsA, UInt#(addrwidth) numColumnsA,
		       ObjectPointer pointerB, UInt#(addrwidth) numRowsB, UInt#(addrwidth) numColumnsB,
		       ObjectPointer pointerC) if (dotprodCount == 0);
      XYRangeConfig#(UInt#(addrwidth)) xycfg = XYRangeConfig {xbase: 0, xlimit: numRowsA, xstep: 1,
							      ybase: 0, ylimit: numRowsB, ystep: fromInteger(n) };
      descriptorA <= MatrixDescriptor { pointer: pointerA, base: 0, numRows: numRowsA, numColumns: numColumnsA};
      descriptorB <= MatrixDescriptor { pointer: pointerB, base: 0, numRows: numRowsB, numColumns: numColumnsB};
      descriptorC <= MatrixDescriptor { pointer: pointerC, base: 0, numRows: numRowsA, numColumns: numRowsB};
      dotprodCount <= numRowsA*numRowsB;

      if (verbose) $display("mm pointerA=%d pointerB=%d pointerC=%d\n", pointerA, pointerB, pointerC);
      if (verbose) $display("mm.start ra=%d ca=%d rb=%d cb=%d dotprodCount=%d", numRowsA, numColumnsA, numRowsB, numColumnsB, dotprodCount);
      if (verbose) $display($format(fshow("mm.start ")+fshow(xycfg)));
      xypipeifc.start(xycfg);
      for (Integer i = 0; i < valueOf(N); i = i + 1) begin
	 fxdotprods[i].numElts <= truncate(numColumnsA);
      end
   endmethod
   method ActionValue#(Bool) finish();
      if (verbose) $display("mm.finish()");
      doneFifo.deq();
      return True;
   endmethod

   interface ObjectWriteClient dmaClient = sinkC.dmaClient;
endmodule

interface DramMatrixMultiply#(numeric type n, numeric type dmasz);
   interface Vector#(TAdd#(N,1), ObjectReadClient#(dmasz)) readClients;
   interface Vector#(1, ObjectWriteClient#(dmasz)) writeClients;
   method Action start(ObjectPointer pointerA, UInt#(ObjectOffsetSize) numRowsA, UInt#(ObjectOffsetSize) numColumnsA,
		       ObjectPointer pointerB, UInt#(ObjectOffsetSize) numRowsB, UInt#(ObjectOffsetSize) numColumnsB,
		       ObjectPointer pointerC);
   method ActionValue#(Bool) finish();
   method Bit#(32) dbg();
endinterface

(* synthesize *)
module [Module] mkDramMatrixMultiply(DramMatrixMultiply#(N,TMul#(N,32)));
   Vector#(TAdd#(N,1), DmaVectorSource#(DmaSz, Vector#(N,Float))) vfsources <- replicateM(mkDmaVectorSource());
   Vector#(1, VectorSource#(DmaSz, Vector#(N,Float))) xvfsources = cons(vfsources[0].vector, nil);
   Vector#(N, VectorSource#(DmaSz, Vector#(N,Float))) yvfsources = takeAt(1, map(dmaVectorSourceVector, vfsources));
   DmaMatrixMultiplyIfc#(ObjectOffsetSize,DmaSz) dmaMMF <- mkDmaMatrixMultiply(xvfsources, yvfsources, mkDmaVectorSink);
   interface Vector readClients = map(getSourceReadClient, vfsources);
   interface Vector writeClients = cons(dmaMMF.dmaClient, nil);
   method start = dmaMMF.start;
   method finish = dmaMMF.finish;
   method Bit#(32) dbg();
      Bit#(32) d = 0;
      d[0] = pack(vfsources[0].vector.pipe.notEmpty());
      d[1] = pack(vfsources[1].vector.pipe.notEmpty());
      return d;
   endmethod
endmodule

interface Mm#(numeric type n);
   interface MmRequest mmRequest;
   interface TimerRequest timerRequest;
   interface Vector#(TAdd#(1,N), ObjectReadClient#(TMul#(32,N))) readClients;
   interface Vector#(1, ObjectWriteClient#(TMul#(32,n))) writeClients;
endinterface

module [Module] mkMm#(MmIndication ind, TimerIndication timerInd)(Mm#(N))
   provisos (Add#(1,a__,N),
	     Add#(N,0,n),
	     Mul#(N,32,DmaSz)
      );

   let n = valueOf(n);

   DramMatrixMultiply#(N, TMul#(N,32)) dmaMMF <- mkDramMatrixMultiply();

   Reg#(Bit#(64)) mmfCycles <- mkReg(0);
   rule countMmfCycles;
      mmfCycles <= mmfCycles + 1;
   endrule

   FIFOF#(Bool) busyFifo <- mkFIFOF();
   rule mmfDone;
      let d <- dmaMMF.finish();
      busyFifo.deq();
      ind.mmfDone(mmfCycles);
   endrule

   FIFOF#(Bool) timerRunning <- mkFIFOF();
   Reg#(Bit#(64)) cycleCount <- mkReg(0);
   Reg#(Bit#(64)) idleCount <- mkReg(0);
   rule countCycles if (timerRunning.notEmpty());
      cycleCount <= cycleCount + 1;
      if (!busyFifo.notEmpty())
	 idleCount <= idleCount + 1;
   endrule

   interface TimerRequest timerRequest;
         method Action startTimer() if (!timerRunning.notEmpty());
	 cycleCount <= 0;
	 idleCount <= 0;
	 timerRunning.enq(True);
      endmethod
      method Action stopTimer();
	 timerRunning.deq();
	 timerInd.elapsedCycles(cycleCount, idleCount);
      endmethod
   endinterface

   interface MmRequest mmRequest;
      method Action mmf(Bit#(32) h1, Bit#(32) r1, Bit#(32) c1,
			Bit#(32) h2, Bit#(32) r2, Bit#(32) c2,
			Bit#(32) h3);
	 dmaMMF.start(h1, unpack(extend(r1)), unpack(extend(c1)),
		      h2, unpack(extend(r2)), unpack(extend(c2)),
		      h3);
	 mmfCycles <= 0;
	 busyFifo.enq(True);
      endmethod
   endinterface   

   interface Vector readClients = dmaMMF.readClients;
   interface Vector writeClients =  dmaMMF.writeClients;

endmodule
