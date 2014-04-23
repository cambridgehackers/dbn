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
import MIMO::*;
import DefaultValue::*;
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

interface DotProdServer#(numeric type n);
   interface Reg#(UInt#(20)) numElts;
   interface Put#(Tuple2#(Vector#(n,Float),Vector#(n,Float))) request;
   interface PipeOut#(Float) pipe;
endinterface

typedef 1 NUM_MACS;

(* synthesize *)
module [Module] mkDotProdServer#(UInt#(TLog#(K)) label)(DotProdServer#(N));

   let n = valueOf(N);
   Bool verbose = False;

   Reg#(UInt#(20)) numEltsReg <- mkReg(0);
   Reg#(UInt#(20)) countInReg <- mkReg(0);
   Vector#(N,FIFOF#(Tuple3#(Bool,Float,Float))) abfifos <- replicateM(mkFIFOF());
   Vector#(N,FIFOF#(Bool)) lastFifos <- replicateM(mkFIFOF());
   Vector#(N, PipeOut#(Tuple3#(Bool,Float,Float))) abpipes = map(toPipeOut,abfifos);

   Vector#(NUM_MACS, Server#(Tuple4#(Maybe#(Float), Float, Float), Tuple2#(Float,Exception))) macs <- replicateM(mkFloatMac(defaultValue));
   Vector#(1, FIFOF#(Bit#(TLog#(N)))) chanFifo <- replicateM(mkFIFOF());
   Vector#(N, FIFO#(Float)) accumFifo <- replicateM(mkFIFO());
   Vector#(N, FIFOF#(Float)) resultFifos <- replicateM(mkFIFOF());

   for (Integer i = 0; i < valueOf(N); i = i + 1) begin
      rule mul;
	 let v = abpipes[i].first;
	 abpipes[i].deq;
	 let isFirst = tpl_1(v);
	 let x = tpl_2(v);
	 let y = tpl_3(v);

	 Maybe#(Float) accum = tagged Invalid;
	 if (!isFirst) begin
	    accum = tagged Valid accumFifo[i].first();
	    accumFifo[i].deq();
	 end
	 Integer mac_number = i;
	 if (valueOf(N) > valueOf(NUM_MACS)) begin
	    chanFifo[0].enq(fromInteger(i));
	    mac_number = 0;
	 end
         macs[mac_number].request.put(tuple3(accum, x, y));
	 if (verbose) $display($format(fshow("label=")+fshow(label)+fshow(" dotprod x=") + fshow(x) + fshow(" y=") + fshow(y)
				       + fshow(" isFirst=") + fshow(isFirst) + fshow(" accum=")+fshow(pack(accum))));
      endrule
   end

   for (Integer i = 0; i < valueOf(N); i = i + 1) begin
      rule macout if ((valueOf(N) == valueOf(NUM_MACS)) || (chanFifo[0].first() == fromInteger(i)));
	 Integer mac_number = i;
	 if (valueOf(N) > valueOf(NUM_MACS)) begin
	    chanFifo[0].deq();
	    mac_number = 0;
	 end
	 let isLast = lastFifos[i].first();
	 lastFifos[i].deq();
	 Float vi;
	 let resp <- macs[mac_number].response.get();
	 vi = tpl_1(resp);
	 if (!isLast) begin
	    accumFifo[i].enq(vi);
	 end
	 else begin
	    resultFifos[i].enq(vi);
	    if (verbose) $display(fshow("label=")+fshow(label)+fshow(" isLast=")+fshow(isLast)+fshow(" mul=") + fshow(vi));
	 end
      endrule
   end

   Vector#(N, PipeOut#(Float)) resultPipes = map(toPipeOut, resultFifos);
   PipeOut#(Vector#(N,Float)) resultPipe <- mkJoinVector(id, resultPipes);
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
	 for (Integer i = 0; i < valueOf(N); i = i + 1) begin
	    let avec = tpl_1(tpl);
	    let bvec = tpl_2(tpl);
	    abfifos[i].enq(tuple3(isFirst,avec[i], bvec[i]));
	    lastFifos[i].enq(isLast);
	 end
      endmethod
   endinterface : request
   interface PipeOut pipe = dotpipe;
   interface Reg numElts = numEltsReg;
endmodule : mkDotProdServer


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

/*!
 * Multiplies two matrices A and B and writes the result to memory.
 * Fetches J rows at a time from A and K rows at a time from B.
 * Each cycle, it can fetch N elements of a row or column.
 * 
 * Just considering memory bandwidth, every J+K cycles it is ready to perform J*K*N multiply accumulates.
 *
 * Currently, requires J == 1.
 */
module [Module] mkDmaMatrixMultiply#(Vector#(J, VectorSource#(dsz, Vector#(N, Float))) sourceA,
				     Vector#(K, VectorSource#(dsz, Vector#(N, Float))) sourceB,
				     function Module#(DmaVectorSink#(dsz, Vector#(N, Float))) mkSink(PipeOut#(Vector#(N, Float)) pipe_in)
				     )(DmaMatrixMultiplyIfc#(addrwidth, dsz))
   provisos (Add#(a__,ObjectOffsetSize,addrwidth)
	     , Add#(N,n__,K)
	     , Mul#(N,m__,K)
	     , Add#(1,0,J)
	     , Log#(N,nshift)
	     , FShow#(Float)
	     , Arith#(Float)
	     , Bits#(Vector#(N, Float), dsz)
	     , Bits#(MatrixDescriptor#(UInt#(addrwidth)), mdsz)
	     , Bits#(Tuple2#(UInt#(addrwidth), UInt#(addrwidth)), tplsz)
	     , Add#(b__, 20, addrwidth)
      );

   let n = valueOf(N);
   let k = valueOf(K);
   let nshift = valueOf(nshift);
   Bool verbose = False;
   Bool verbose1 = True;

   Reg#(Bool) doneReg <- mkReg(False);
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorA <- mkReg(unpack(0));
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorB <- mkReg(unpack(0));
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorC <- mkReg(unpack(0));
   Reg#(UInt#(addrwidth)) dotprodCount <- mkReg(0);

   Vector#(K, PipeOut#(Vector#(N,Float))) aPipes <- mkForkVector(sourceA[0].pipe);

   function Module#(DotProdServer#(N)) mkFxDotProd(Integer i);
      return mkDotProdServer(fromInteger(i));
   endfunction
   Vector#(K, DotProdServer#(N)) fxdotprods <- genWithM(mkFxDotProd);
   for (Integer i = 0; i < k; i = i+1)
      rule connectDotProd;
	 let a = aPipes[i].first();
	 aPipes[i].deq();
	 let b = sourceB[i].pipe.first();
	 sourceB[i].pipe.deq();
	 fxdotprods[i].request.put(tuple2(a, b));
      endrule

   MIMOConfiguration mimoCfg = defaultValue;
   MIMO#(K,N,K,Float) dfifo <- mkMIMO(mimoCfg);
   let sinkC <- mkSink(toPipeOut(dfifo));

   XYRangePipeIfc#(UInt#(addrwidth)) xypipeifc <- mkXYRangePipeOut();

   Vector#(TAdd#(K,2), PipeOut#(Tuple2#(UInt#(addrwidth),UInt#(addrwidth)))) xypipes <- mkForkVector(xypipeifc.pipe);

   Reg#(Bool) running <- mkReg(False);
   FIFOF#(Bool) doneFifo <- mkFIFOF();

   Reg#(UInt#(32)) cycles <- mkReg(0);
   rule countCycles;
      cycles <= cycles+1;
   endrule

   for (Integer i = 0; i < k; i = i + 1) begin
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

	 UInt#(TLog#(K)) in = fromInteger(i);
	 if (i == 0) begin
	    sinkC.vector.start(descriptorC.pointer, pack(truncate(startC>>nshift)), fromInteger(k/n));
 	    if (verbose || verbose1) $display($format(fshow(cycles)+fshow("      sinkC[")+fshow(in)+fshow("].start")+fshow(startC)));
	 end
	 if (i == 0) begin
	    sourceA[0].start(descriptorA.pointer, pack(truncate(startA>>nshift)), pack(truncate(descriptorA.numColumns>>nshift)));
	    if (verbose || verbose1) $display($format(fshow(cycles)+fshow("    sourceA[0].start")+fshow(startA)));
	 end
	 sourceB[i].start(descriptorB.pointer, pack(truncate(startB>>nshift)), pack(truncate(descriptorB.numColumns>>nshift)));
	 if (verbose || verbose1) $display($format(fshow(cycles)+fshow("    sourceB[")+fshow(in)+fshow("].start")+fshow(startB)));
      endrule
      if (i == 0)
	 rule finishSourceA;
	    if (verbose || verbose1) $display($format(fshow(cycles)+fshow("    sourceA[0].finish ")));
	    let b <- sourceA[0].finish();
	 endrule
      rule finishSourceB;
	 UInt#(TLog#(K)) in = fromInteger(i);
	 if (verbose || verbose1) $display($format(fshow(cycles)+fshow("    sourceB[")+fshow(in)+fshow("].finish")));
	 let b <- sourceB[i].finish();
      endrule
   end

   rule dotProdValue;
      Tuple2#(UInt#(addrwidth),UInt#(addrwidth)) xy = xypipes[k].first;
      xypipes[k].deq;
      Vector#(K,Float) vs;
      for (Integer i = 0; i < k; i = i + 1) begin
	 let v = fxdotprods[i].pipe.first;
	 fxdotprods[i].pipe.deq;
	 let xyi = tuple2(tpl_1(xy), tpl_2(xy)+fromInteger(i));
	 if (verbose) $display($format(fshow(cycles)+fshow("    dotprodvalue xy=")+fshow(xyi)+fshow(" dotprod=")+fshow(v)));
	 vs[i] = v;
      end
      dfifo.enq(fromInteger(k), vs);
   endrule

   rule sinkDone;
      // each time we write a burst of k values via sinkC
      let xy = xypipes[k+1].first;
      xypipes[k+1].deq;
      let b <- sinkC.vector.finish();
      let c = dotprodCount-fromInteger(k);
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
							      ybase: 0, ylimit: numRowsB, ystep: fromInteger(k) };
      descriptorA <= MatrixDescriptor { pointer: pointerA, base: 0, numRows: numRowsA, numColumns: numColumnsA};
      descriptorB <= MatrixDescriptor { pointer: pointerB, base: 0, numRows: numRowsB, numColumns: numColumnsB};
      descriptorC <= MatrixDescriptor { pointer: pointerC, base: 0, numRows: numRowsA, numColumns: numRowsB};
      dotprodCount <= numRowsA*numRowsB;

      if (verbose) $display("mm pointerA=%d pointerB=%d pointerC=%d\n", pointerA, pointerB, pointerC);
      if (verbose) $display("mm.start ra=%d ca=%d rb=%d cb=%d dotprodCount=%d", numRowsA, numColumnsA, numRowsB, numColumnsB, dotprodCount);
      if (verbose) $display($format(fshow("mm.start ")+fshow(xycfg)));
      xypipeifc.start(xycfg);
      for (Integer i = 0; i < k; i = i + 1) begin
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
   interface Vector#(TAdd#(K,1), ObjectReadClient#(dmasz)) readClients;
   interface Vector#(1, ObjectWriteClient#(dmasz)) writeClients;
   method Action start(ObjectPointer pointerA, UInt#(ObjectOffsetSize) numRowsA, UInt#(ObjectOffsetSize) numColumnsA,
		       ObjectPointer pointerB, UInt#(ObjectOffsetSize) numRowsB, UInt#(ObjectOffsetSize) numColumnsB,
		       ObjectPointer pointerC);
   method ActionValue#(Bool) finish();
   method Bit#(32) dbg();
endinterface

(* synthesize *)
module [Module] mkDramMatrixMultiply(DramMatrixMultiply#(N,TMul#(N,32)));
   Vector#(TAdd#(K,J), DmaVectorSource#(DmaSz, Vector#(N,Float))) vfsources <- replicateM(mkDmaVectorSource());
   Vector#(J, VectorSource#(DmaSz, Vector#(N,Float))) xvfsources = cons(vfsources[0].vector, nil);
   Vector#(K, VectorSource#(DmaSz, Vector#(N,Float))) yvfsources = takeAt(1, map(dmaVectorSourceVector, vfsources));
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
   interface Vector#(TAdd#(K,J), ObjectReadClient#(TMul#(32,N))) readClients;
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
