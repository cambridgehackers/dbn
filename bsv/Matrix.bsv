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
import Assert::*;

interface DotProdServer#(numeric type n);
   interface Reg#(UInt#(20)) numElts;
   interface Put#(Tuple2#(Vector#(n,Float),Vector#(n,Float))) request;
   interface PipeOut#(Float) pipe;
endinterface

(* synthesize *)
module [Module] mkDotProdServer#(UInt#(TLog#(TMul#(J,K))) label)(DotProdServer#(N));

   let n = valueOf(N);
   let add_depth = valueOf(FP_ADD_DEPTH);
   let mul_depth = valueOf(FP_MUL_DEPTH);
   Bool verbose = False; //label==0;
   
   Reg#(Bit#(TAdd#(TLog#(FP_ADD_DEPTH),1))) initCnt <- mkReg(0);
   FIFO#(void) initCtrl <- mkSizedFIFO(1);
   
   Reg#(UInt#(20)) numEltsReg <- mkReg(0);
   Reg#(UInt#(20)) countInReg <- mkReg(0);

   Vector#(N, FloatAlu#(FP_MUL_DEPTH)) muls <- replicateM(mkFloatMultiplier(defaultValue));
   Vector#(N, FloatAlu#(FP_ADD_DEPTH)) adders <- replicateM(mkFloatAdder(defaultValue));
   
   Vector#(N,FIFOF#(Tuple2#(Float,Float))) abfifos <- replicateM(mkFIFOF());
   Vector#(N,FIFOF#(Bool)) lastFifos <- replicateM(mkSizedFIFOF(mul_depth));

   Vector#(N, Reg#(Bit#(TAdd#(TLog#(FP_ADD_DEPTH),1)))) drainCnts <- replicateM(mkReg(0));
   Vector#(N, Reg#(Bool)) drained <- replicateM(mkReg(False));
   
   Reg#(Maybe#(Float)) accum <- mkReg(Nothing);
   FIFOF#(Float) dotfifo <- mkFIFOF;

   function Bit#(TLog#(N)) i_v(Integer i) = fromInteger(i);
   
   rule init if (initCnt > 0);
      for(Integer i = 0; i < n; i = i + 1)
	 adders[i].request.put(tuple2(0,0));
      initCnt <= initCnt - 1;
      if (verbose) $display($format(fshow("label=")+fshow(label)+fshow(" initCnt=")+fshow(initCnt)));
   endrule
   
   for (Integer i = 0; i < n; i = i + 1)
      rule mul;
	 // this rule could be folded into the 'put' method to reduce latency
	 match {.x,.y} <- toGet(abfifos[i]).get;
	 muls[i].request.put(tuple2(x, y));
	 if (verbose) $display($format(fshow("label=")+fshow(label)+fshow(" mul=")+fshow(i_v(i))));
      endrule

   for (Integer i = 0; i < n; i = i + 1) 
      rule acc if (drainCnts[i] == 0 && initCnt == 0);
	 match {.resp,.*} <- muls[i].response.get();
	 match {.acc,.*} <- adders[i].response.get;
	 adders[i].request.put(tuple2(resp,acc));
	 let last <- toGet(lastFifos[i]).get;
	 if (last) begin 
	    if (i>0)
	       drainCnts[i] <= fromInteger(add_depth);
	    else
	       drainCnts[i] <= fromInteger((add_depth*2)-1); // is this correct??
	 end
	 if (verbose) $display($format(fshow("label=")+fshow(label)+fshow(" acc=")+fshow(i_v(i))+fshow(" last=")+fshow(last)));
      endrule
      
   for (Integer i = 1; i < n; i = i + 1) 
      rule gather if (drainCnts[i] > 0);
	 let new_cnt = drainCnts[i]-1;
	 drained[i] <= (new_cnt==0);
	 drainCnts[i] <= new_cnt;
	 match {.a,.*} <- adders[0].response.get;
	 match {.b,.*} <- adders[i].response.get;
	 adders[0].request.put(tuple2(a,b));
	 if (verbose) $display($format(fshow("label=")+fshow(label)+fshow(" gather=")+fshow(i_v(i))+fshow(" drainCnt=")+fshow(drainCnts[i])));
      endrule
   
   // the reference-guide says this should work, but it doesn't compile:
   // let gathered = and(tail(drained));
   function Bool is_true(Bool b) = b;
   let gathered = all(is_true, readVReg(tail(drained)));
   // this will only work correctly when add_depth is odd
   rule drain if (gathered && drainCnts[0] > 0);
      let new_cnt = drainCnts[0]-1;
      match {.a,.*} <- adders[0].response.get;
      drainCnts[0] <= new_cnt;
      let enq = False;
      if(accum matches tagged Valid .v) begin
	 adders[0].request.put(tuple2(a,v));
	 dynamicAssert(new_cnt > 0, "mkDotProdServer::drain");
	 enq = True;
	 accum <= tagged Invalid;
      end 
      else begin
	 if (new_cnt == 0) begin
	    dotfifo.enq(a);
	    initCtrl.deq;
	 end
	 else 
	    accum <= tagged Valid a;
      end
      if (verbose) $display($format(fshow("label=")+fshow(label)+fshow(" drain0=")+fshow(enq)+fshow(" drainCnt=")+fshow(drainCnts[0])));
   endrule
         
   PipeOut#(Float) dotpipe = toPipeOut(dotfifo);
   interface Put request;
      method Action put(Tuple2#(Vector#(N,Float),Vector#(N,Float)) tpl);
	 if (countInReg == 0) begin
	    initCtrl.enq(?);
	    initCnt <= fromInteger(add_depth);
	 end
	 let c = countInReg+fromInteger(n);
	 Bool isLast = (c >= numEltsReg);
	 if (isLast) begin
	    c = 0;
	 end
	 countInReg <= c;
	 match { .avec, .bvec } = tpl;
	 function Action enqvalues(Integer i);
	    action
	       abfifos[i].enq(tuple2(avec[i], bvec[i]));
	       lastFifos[i].enq(isLast);
	       //if (verbose) $display($format(fshow("label=")+fshow(label)+fshow(" countInReg=(")+fshow(countInReg)+fshow("/")+fshow(numEltsReg)+fshow(") dotprod x=") + fshow(avec[i]) + fshow(" y=") + fshow(bvec[i])));
	    endaction
	 endfunction
	 Vector#(N, Integer) indices = genVector();
	 mapM_(enqvalues, indices);
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
   interface Vector#(J, ObjectWriteClient#(dsz)) writeClients;
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
 */
module [Module] mkDmaMatrixMultiply#(Vector#(J, VectorSource#(dsz, Vector#(N, Float))) sourceA,
				     Vector#(K, VectorSource#(dsz, Vector#(N, Float))) sourceB,
				     function Module#(DmaVectorSink#(dsz, Vector#(N, Float))) mkSink(PipeOut#(Vector#(N, Float)) pipe_in)
				     )(DmaMatrixMultiplyIfc#(addrwidth, dsz))
   provisos (  Add#(N,n__,K)
	     , Mul#(N,m__,K)
	     , Add#(1,o__,J)
	     , Log#(N,nshift)
	     , FShow#(Float)
	     , Arith#(Float)
	     , Bits#(Vector#(N, Float), dsz)
	     , Bits#(MatrixDescriptor#(UInt#(addrwidth)), mdsz)
	     , Bits#(Tuple2#(UInt#(addrwidth), UInt#(addrwidth)), tplsz)
	     , Add#(b__, 20, addrwidth)
	     , Add#(a__, addrwidth, 40)
      );

   let n = valueOf(N);
   let jj = valueOf(J);
   let kk = valueOf(K);
   let nshift = valueOf(nshift);
   Bool verbose = False;
   Bool verbose1 = False;
   Bool timing = True;
					
   Reg#(Bool) doneReg <- mkReg(False);
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorA <- mkReg(unpack(0));
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorB <- mkReg(unpack(0));
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorC <- mkReg(unpack(0));
   Reg#(UInt#(addrwidth)) dotprodCount <- mkReg(0);

   Vector#(J, Vector#(K, PipeOut#(Vector#(N,Float)))) aPipes  <- mapM(mkSizedForkVector(valueOf(SourceBufferSize)), map(vectorSourcePipe, sourceA));
   Vector#(K, Vector#(J, PipeOut#(Vector#(N,Float)))) bPipesT <- mapM(mkForkVector, map(vectorSourcePipe, sourceB));
   Vector#(J, Vector#(K, PipeOut#(Vector#(N,Float)))) bPipes  =  transpose(bPipesT);

   function Module#(DotProdServer#(N)) mkFxDotProd(Integer i);
      return mkDotProdServer(fromInteger(i));
   endfunction
   Vector#(TMul#(J,K), DotProdServer#(N)) fxdotprods <- genWithM(mkFxDotProd);
//`define LOCKSTEP
`ifndef LOCKSTEP
   for (Integer k = 0; k < kk; k = k+1) begin
      for (Integer j = 0; j < jj; j = j + 1) begin
	 rule connectDotProd;
	    let index = j*kk+k;
	    let a <- toGet(aPipes[j][k]).get();
	    let b <- toGet(bPipes[j][k]).get();
	    int jint = fromInteger(j);
	    int kint = fromInteger(k);
	    //$display($format(fshow("connectDotProd pos=")+fshow(tuple2(jint,kint))+fshow(" a=")+fshow(pack(a))+fshow(" b=")+fshow(pack(b))));
	    fxdotprods[index].request.put(tuple2(a, b));
	 endrule
      end
   end
`else
   rule connectDotProd;
      for (Integer k = 0; k < kk; k = k+1) begin
	 for (Integer j = 0; j < jj; j = j + 1) begin
	    let index = j*kk+k;
	    let a <- toGet(aPipes[j][k]).get();
	    let b <- toGet(bPipes[j][k]).get();
	    int jint = fromInteger(j);
	    int kint = fromInteger(k);
	    //$display($format(fshow("connectDotProd pos=")+fshow(tuple2(jint,kint))+fshow(" a=")+fshow(pack(a))+fshow(" b=")+fshow(pack(b))));
	    fxdotprods[index].request.put(tuple2(a, b));
	 end
      end
   endrule
`endif
   MIMOConfiguration mimoCfg = defaultValue;
   MIMO#(K,N,TAdd#(K,N),Float) dfifo <- mkMIMO(mimoCfg);
   Vector#(J, MIMO#(K,N,TAdd#(K,N),Float)) dfifos <- replicateM(mkMIMO(mimoCfg));
   let sinks <- mapM(mkSink, map(toPipeOut, dfifos));

   XYRangePipeIfc#(UInt#(addrwidth)) indexpipeifc <- mkXYRangePipeOut();
   XYRangePipeIfc#(UInt#(addrwidth)) offsetpipeA <- mkXYRangePipeOut();
   XYRangePipeIfc#(UInt#(addrwidth)) offsetpipeB <- mkXYRangePipeOut();
   XYRangePipeIfc#(UInt#(addrwidth)) offsetpipeC <- mkXYRangePipeOut();

   Vector#(TAdd#(TAdd#(J,J),K), PipeOut#(Tuple2#(UInt#(addrwidth),UInt#(addrwidth)))) indexpipes <- mkSizedForkVector(valueOf(SourceBufferSize), indexpipeifc.pipe);
   Vector#(J, PipeOut#(Tuple2#(UInt#(addrwidth),UInt#(addrwidth))))        offsetpipesA <- mkSizedForkVector(valueOf(SourceBufferSize), offsetpipeA.pipe);
   Vector#(K, PipeOut#(Tuple2#(UInt#(addrwidth),UInt#(addrwidth))))        offsetpipesB <- mkSizedForkVector(valueOf(SourceBufferSize), offsetpipeB.pipe);
   Vector#(J, PipeOut#(Tuple2#(UInt#(addrwidth),UInt#(addrwidth))))        offsetpipesC <- mkSizedForkVector(valueOf(SourceBufferSize), offsetpipeC.pipe);

   Reg#(Bool) running <- mkReg(False);
   FIFOF#(Bool) doneFifo <- mkFIFOF();

   Reg#(UInt#(32)) cycles <- mkReg(0);
   rule countCycles;
      cycles <= cycles+1;
   endrule

   Vector#(J, Reg#(UInt#(addrwidth))) startAOffset <- replicateM(mkReg(0));
   Vector#(K, Reg#(UInt#(addrwidth))) startBOffset <- replicateM(mkReg(0));
   Vector#(J, Reg#(UInt#(addrwidth))) startCOffset <- replicateM(mkReg(0));
   for (Integer k = 0; k < kk; k = k + 1) begin
      rule startDotProds;
	 Tuple2#(UInt#(addrwidth),UInt#(addrwidth)) index <- toGet(indexpipes[k]).get();
	 match { .unusedB, .startBBase } <- toGet(offsetpipesB[k]).get();
	 
	 int kint = fromInteger(k);

	 let row = tpl_1(index);
	 let col = tpl_2(index)+fromInteger(k);
	 
	 let startB = startBBase + startBOffset[k];
	 
	 if (timing || verbose) $display($format(fshow(cycles)+fshow("    startB index=")+fshow(tuple2(row,col))
	    +fshow(" startB=")+fshow(startB)
	    +fshow(" k=")+fshow(kint)));

	 if (verbose || verbose1) $display($format(fshow(cycles)+fshow("    sourceB[")+fshow(kint)+fshow("].start")+fshow(startB)));

	 sourceB[k].start(descriptorB.pointer, pack(extend(startB>>nshift)), pack(extend(descriptorB.numColumns>>nshift)));
      endrule
      rule finishSourceB;
	 UInt#(TLog#(K)) in = fromInteger(k);
	 int kint = fromInteger(k);
	 if (verbose || verbose1) $display($format(fshow(cycles)+fshow("    sourceB[")+fshow(kint)+fshow("].finish")));
	 let b <- sourceB[k].finish();
      endrule
   end
   for (Integer j = 0; j < jj; j = j + 1) begin

      rule startSourceAndSink;
	 Tuple2#(UInt#(addrwidth),UInt#(addrwidth)) index <- toGet(indexpipes[j+kk]).get();

	 let row = tpl_1(index)+fromInteger(j);
	 let col = tpl_2(index);

	 match { .startABase, .unusedA } <- toGet(offsetpipesA[j]).get();
	 match { .startCBase, .offsetC } <- toGet(offsetpipesC[j]).get();
	 let startA = startABase + startAOffset[j];
	 let startC = startCBase + startCOffset[j] + offsetC;

	 int jint = fromInteger(j);
	 if (timing || verbose) $display($format(fshow(cycles)+fshow("    start A/C index=")+fshow(tuple2(row,col))
	    +fshow(" startA=")+fshow(startA)
	    +fshow(" startC=")+fshow(startC)
	    +fshow(" startC>>nshift=")+fshow(startC>>nshift)
	    +fshow(" j=")+fshow(jint)));

	 sourceA[j].start(descriptorA.pointer, pack(extend(startA>>nshift)), pack(extend(descriptorA.numColumns>>nshift)));
	 if (verbose || verbose1) $display($format(fshow(cycles)+fshow("    sourceA[")+fshow(jint)+fshow("].start")+fshow(startA)));
	 sinks[j].vector.start(descriptorC.pointer, pack(extend(startC>>nshift)), fromInteger(kk/n));
	 if (verbose || verbose1) $display($format(fshow(cycles)+fshow("      sinks[")+fshow(jint)+fshow("].start")+fshow(startC)));
      endrule

      rule finishSourceA;
	 if (verbose || verbose1) $display($format(fshow(cycles)+fshow("    sourceA[0].finish ")));
	 let b <- sourceA[j].finish();
      endrule

      rule dotProdValue;
	 Tuple2#(UInt#(addrwidth),UInt#(addrwidth)) index <- toGet(indexpipes[jj+kk+j]).get();
	 Vector#(K,Float) vs;
	 for (Integer k = 0; k < kk; k = k + 1) begin
	    let dpnum = j*kk+k;
	    let v <- toGet(fxdotprods[dpnum].pipe).get();
	    //let indexi = tuple2(tpl_1(index), tpl_2(index)+fromInteger(k));
	    //if (verbose) $display($format(fshow(cycles)+fshow("    dotprodvalue index=")+fshow(indexi)+fshow(" dotprod=")+fshow(v)));
	    int jint = fromInteger(j);
	    int kint = fromInteger(k);
	    if (verbose1) $display($format(fshow(cycles)+fshow("    dotprodvalue pos=")+fshow(index)+fshow(tuple2(jint,kint))+fshow(" dotprod=")+fshow(pack(v))));
	    vs[k] = v;
	 end
	 dfifos[j].enq(fromInteger(kk), vs);
      endrule

      rule sinkDone;
	 // each time we write a burst of k values via sinks
	 //let index <- toGet(indexpipes[jj+kk+1]).get();
	 let b <- sinks[j].vector.finish();
	 let c = dotprodCount-fromInteger(kk);
	 int jint = fromInteger(j);
	 if (verbose1) $display($format(fshow(cycles)+fshow("    sinkDone c")+fshow(c)+fshow(" j=")+fshow(jint)));
	 dotprodCount <= c;
	 if (c == 0) begin
	    running <= False;
	    doneFifo.enq(?);
	 end
      endrule
   end

   method Action start(ObjectPointer pointerA, UInt#(addrwidth) numRowsA, UInt#(addrwidth) numColumnsA,
		       ObjectPointer pointerB, UInt#(addrwidth) numRowsB, UInt#(addrwidth) numColumnsB,
		       ObjectPointer pointerC) if (!running);
      XYRangeConfig#(UInt#(addrwidth)) indexcfg  = XYRangeConfig {xbase: 0, xlimit: numRowsA, xstep: fromInteger(jj),
								  ybase: 0, ylimit: numRowsB, ystep: fromInteger(kk) };
      XYRangeConfig#(UInt#(addrwidth)) offsetcfgA = XYRangeConfig {xbase: 0, xlimit: numRowsA*numColumnsA, xstep: numColumnsA*fromInteger(jj),
								  ybase: 0, ylimit: numRowsB, ystep: fromInteger(kk) };
      XYRangeConfig#(UInt#(addrwidth)) offsetcfgB = XYRangeConfig {xbase: 0, xlimit: numRowsA, xstep: fromInteger(jj),
								  ybase: 0, ylimit: numRowsB*numColumnsB, ystep: fromInteger(kk)*numColumnsB };
      XYRangeConfig#(UInt#(addrwidth)) offsetcfgC = XYRangeConfig {xbase: 0, xlimit: numRowsA*numRowsB, xstep: numRowsB*fromInteger(jj),
								  ybase: 0, ylimit: numRowsB, ystep: fromInteger(kk) };
      descriptorA <= MatrixDescriptor { pointer: pointerA, base: 0, numRows: numRowsA, numColumns: numColumnsA};
      descriptorB <= MatrixDescriptor { pointer: pointerB, base: 0, numRows: numRowsB, numColumns: numColumnsB};
      descriptorC <= MatrixDescriptor { pointer: pointerC, base: 0, numRows: numRowsA, numColumns: numRowsB};
      dotprodCount <= numRowsA*numRowsB;
      running <= True;

      if (verbose) $display("mm pointerA=%d pointerB=%d pointerC=%d\n", pointerA, pointerB, pointerC);
      if (verbose) $display("mm.start ra=%d ca=%d rb=%d cb=%d dotprodCount=%d", numRowsA, numColumnsA, numRowsB, numColumnsB, dotprodCount);
      if (verbose) $display($format(fshow("mm.start ")+fshow(indexcfg)));
      indexpipeifc.start(indexcfg);
      offsetpipeA.start(offsetcfgA);
      offsetpipeB.start(offsetcfgB);
      offsetpipeC.start(offsetcfgC);
      for (Integer j = 0; j < jj; j = j + 1) begin
	 startAOffset[j] <= fromInteger(j)*numColumnsA;
	 startCOffset[j] <= fromInteger(j)*numRowsB;
      end
      for (Integer k = 0; k < kk; k = k + 1) begin
	 for (Integer j = 0; j < jj; j = j + 1) begin
	    let index = j*kk + k;
	    fxdotprods[index].numElts <= truncate(numColumnsA);
	 end
	 startBOffset[k] <= fromInteger(k)*numColumnsB;
      end
   endmethod
   method ActionValue#(Bool) finish();
      if (verbose) $display("mm.finish()");
      doneFifo.deq();
      return True;
   endmethod

   interface Vector writeClients = map(getSinkWriteClient, sinks);
endmodule

typedef 20 MMSize;

interface DramMatrixMultiply#(numeric type n, numeric type dmasz);
   interface Vector#(TAdd#(K,J), ObjectReadClient#(dmasz)) readClients;
   interface Vector#(J, ObjectWriteClient#(dmasz)) writeClients;
   method Action start(ObjectPointer pointerA, UInt#(MMSize) numRowsA, UInt#(MMSize) numColumnsA,
		       ObjectPointer pointerB, UInt#(MMSize) numRowsB, UInt#(MMSize) numColumnsB,
		       ObjectPointer pointerC);
   method ActionValue#(Bool) finish();
   method Bit#(32) dbg();
endinterface

(* synthesize *)
module [Module] mkDramMatrixMultiply(DramMatrixMultiply#(N,TMul#(N,32)));
   Vector#(TAdd#(K,J), DmaVectorSource#(DmaSz, Vector#(N,Float))) vfsources <- replicateM(mkDmaVectorSource());
   Vector#(J, VectorSource#(DmaSz, Vector#(N,Float))) xvfsources = takeAt(0,          map(dmaVectorSourceVector, vfsources));
   Vector#(K, VectorSource#(DmaSz, Vector#(N,Float))) yvfsources = takeAt(valueOf(J), map(dmaVectorSourceVector, vfsources));
   DmaMatrixMultiplyIfc#(MMSize,DmaSz) dmaMMF <- mkDmaMatrixMultiply(xvfsources, yvfsources, mkDmaVectorSink);
   interface Vector readClients = map(getSourceReadClient, vfsources);
   interface Vector writeClients = dmaMMF.writeClients;
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
   interface Vector#(J, ObjectWriteClient#(TMul#(32,n))) writeClients;
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
	 dmaMMF.start(h1, unpack(truncate(r1)), unpack(truncate(c1)),
		      h2, unpack(truncate(r2)), unpack(truncate(c2)),
		      h3);
	 mmfCycles <= 0;
	 busyFifo.enq(True);
      endmethod
   endinterface   

   interface Vector readClients = dmaMMF.readClients;
   interface Vector writeClients =  dmaMMF.writeClients;

endmodule
