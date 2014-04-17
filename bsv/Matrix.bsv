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

module [Module] mkDotProd#(PipeOut#(Vector#(n,Float)) xpipe, PipeOut#(Vector#(n,Float)) ypipe, UInt#(nwidth) numElts)(PipeOut#(Float))
   provisos (
      Add#(1,a__,n)
      );

   Bool verbose = True;

   PipeOut#(Tuple2#(Vector#(n,Float),Vector#(n,Float))) xypipe <- mkJoin(tuple2, xpipe, ypipe);

   Vector#(n, Server#(Tuple4#(Maybe#(Float), Float, Float, RoundMode), Tuple2#(Float,Exception))) macs <- replicateM(mkFloatMac);
   Vector#(n, FIFO#(Maybe#(Float))) accumFifo <- replicateM(mkFIFO());
   Reg#(UInt#(nwidth)) countInReg <- mkReg(0);
   Reg#(UInt#(nwidth)) countOutReg <- mkReg(0);
   FIFOF#(Float) resultFifo <- mkFIFOF();

   rule mul;
      let v = xypipe.first;
      xypipe.deq;
      let x = tpl_1(v);
      let y = tpl_2(v);

      for (Integer i = 0; i < valueOf(n); i = i + 1) begin
	 Maybe#(Float) accum = tagged Invalid;
	 if (countInReg > 0) begin
	    accum = accumFifo[i].first();
	    accumFifo[i].deq();
	 end
	 if (verbose) $display($format(fshow(" dotprod x=") + fshow(x) + fshow(" y=") + fshow(y) + fshow(" accum=")+fshow(pack(accum))));
         macs[i].request.put(tuple4(accum, x[i], y[i], defaultValue));
	 let c = countInReg + 1;
	 if (c == numElts)
	    c = 0;
	 countInReg <= c;
      end
   endrule

   rule macout;
      Vector#(n,Float) vs;
      let c = countOutReg + 1;
      for (Integer i = 0; i < valueOf(n); i = i + 1) begin
	 let resp <- macs[i].response.get();
	 vs[i] = tpl_1(resp);
	 if (c < numElts) begin
	    accumFifo[i].enq(tagged Valid tpl_1(resp));
	 end
	 else begin
	    // FIXME need to fold the values together, but we're only compiling for n == 1
	    $display("resultFifo.enq %h", tpl_1(resp));
	    if (i == 0) begin
	       resultFifo.enq(tpl_1(resp));
	    end
	 end
      end
      if (c == numElts)
	 c = 0;
      countOutReg <= c;
      if (verbose) $display(fshow("countOutReg=")+fshow(countOutReg)+fshow(" numElts=")+fshow(numElts)+fshow(" mul=") + fshow(vs));
   endrule

   PipeOut#(Float) dotpipe = toPipeOut(resultFifo);
   return dotpipe;
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
   	 $display("XYRangePipe x=%d xlimit=%d y=%d ylimit=%d", x, xlimit, y, ylimit);
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

module [Module] mkDmaMatrixMultiply#(Vector#(1, VectorSource#(dsz, Vector#(1, Float))) sourceA,
				     Vector#(k, VectorSource#(dsz, Vector#(1, Float))) sourceB,
				     function Module#(DmaVectorSink#(dsz, Vector#(1, Float))) mkSink(PipeOut#(Vector#(1, Float)) pipe_in)
				     )(DmaMatrixMultiplyIfc#(addrwidth, dsz))
   provisos (Add#(a__,ObjectOffsetSize,addrwidth)
	     , Add#(1,0,n)
	     , Add#(1,c__,k)
	     , FShow#(Float)
	     , Arith#(Float)
	     , Bits#(Vector#(1, Float), dsz));

   let n = valueOf(n);
   let k = valueOf(k);
   Bool verbose = True;

   Reg#(Bool) doneReg <- mkReg(False);
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorA <- mkReg(unpack(0));
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorB <- mkReg(unpack(0));
   Reg#(MatrixDescriptor#(UInt#(addrwidth))) descriptorC <- mkReg(unpack(0));
   Reg#(UInt#(addrwidth)) dpCount <- mkReg(0);

   Vector#(k, PipeOut#(Vector#(1,Float))) aPipes <- mkForkVector(sourceA[0].pipe);

   function Module#(PipeOut#(Float)) mkFxDotProd(Integer i);
      return mkDotProd(aPipes[i], sourceB[i].pipe, descriptorA.numColumns/fromInteger(n));
   endfunction
   Vector#(n, PipeOut#(Float)) fxdotprods <- genWithM(mkFxDotProd);

   FIFOF#(Vector#(n,Float)) dfifo <- mkFIFOF();
   let sinkC <- mkSink(toPipeOut(dfifo));

   XYRangePipeIfc#(UInt#(addrwidth)) xypipeifc <- mkXYRangePipeOut();

   Vector#(TAdd#(n,1), PipeOut#(Tuple2#(UInt#(addrwidth),UInt#(addrwidth)))) xypipes <- mkForkVector(xypipeifc.pipe);

   Reg#(Bool) running <- mkReg(False);
   FIFOF#(Bool) doneFifo <- mkFIFOF();

   FIFOF#(Bool) waitingForB <- mkFIFOF();
   for (Integer i = 0; i < k; i = i + 1) begin
      rule startDotProd;
	  Tuple2#(UInt#(addrwidth),UInt#(addrwidth)) xy = xypipes[i].first;
	  xypipes[i].deq();

	  let row = tpl_1(xy);
	  let col = tpl_2(xy)+fromInteger(i);

	 if (verbose) $display($format(fshow("startDotProd xy=")+fshow(tuple2(row,col))+fshow(" acols=")+fshow(descriptorA.numColumns)+fshow(" bcols=")+fshow(descriptorB.numColumns)));
	  let startA = row*descriptorA.numColumns/fromInteger(n); // row major
	  let startB = col*descriptorB.numColumns/fromInteger(n); // col major layout (pre-transposed)
	  let startC = (row*descriptorC.numColumns + col) / fromInteger(n); // row major
	  if (verbose) $display($format(fshow("startDotProd xy=")+fshow(tuple2(row,col))
					+fshow(" colsC=")+fshow(descriptorC.numColumns)
					+fshow(" startC=")+fshow(startC)));

	  if (i == 0)
	     sourceA[0].start(descriptorA.pointer, pack(truncate(startA)), pack(truncate(startA + descriptorA.numColumns/fromInteger(n))));
	  sourceB[i].start(descriptorB.pointer, pack(truncate(startB)), pack(truncate(startB + descriptorB.numColumns/fromInteger(n))));
	  if (i == 0)
	     sinkC.vector.start(descriptorC.pointer, pack(truncate(startC)), pack(truncate(startC + fromInteger(k))));
       endrule
       if (i == 0)
	  rule finishSourceA;
	     //$display($format(fshow("sourceA[0].finish ")+sourceA[0].dbg()));
	     let b <- sourceA[0].finish();
	     //waitingForB.enq(True);
	  endrule
      rule finishSourceB;
	 $display("sourceB[%d].finish", i);
	 let b <- sourceB[i].finish();
	 //waitingForB.deq();
      endrule
      rule waiting if (waitingForB.notEmpty);
	 //$display($format(fshow("waiting for B: ")+ sourceB[i].dbg()));
      endrule
      
   end

   rule dotProdValue;
      Tuple2#(UInt#(addrwidth),UInt#(addrwidth)) xy = xypipes[n].first;
      xypipes[n].deq;
      Vector#(n,Float) vs;
      for (Integer i = 0; i < k; i = i + 1) begin
	 let v = fxdotprods[i].first;
	 fxdotprods[i].deq;
	 let xyi = tuple2(tpl_1(xy), tpl_2(xy)+fromInteger(i));
	 if (verbose) $display($format(fshow("dotprodvalue xy=")+fshow(xyi)+fshow(" dotprod=")+fshow(v)));
	 vs[i] = v;
      end
      dfifo.enq(vs);
   endrule

   rule sinkDone;
      // each time we write a burst of k values via sinkC
      let b <- sinkC.vector.finish();
      let c = dpCount-fromInteger(k);
      $display("sinkDone c=%d", c);
      dpCount <= c;
      if (c == 0) begin
	 running <= False;
	 doneFifo.enq(?);
      end
   endrule

   method Action start(ObjectPointer pointerA, UInt#(addrwidth) numRowsA, UInt#(addrwidth) numColumnsA,
		       ObjectPointer pointerB, UInt#(addrwidth) numRowsB, UInt#(addrwidth) numColumnsB,
		       ObjectPointer pointerC) if (dpCount == 0);
      XYRangeConfig#(UInt#(addrwidth)) xycfg = XYRangeConfig {xbase: 0, xlimit: numRowsA, xstep: 1,
							      ybase: 0, ylimit: numRowsB, ystep: fromInteger(n) };
      descriptorA <= MatrixDescriptor { pointer: pointerA, base: 0, numRows: numRowsA, numColumns: numColumnsA};
      descriptorB <= MatrixDescriptor { pointer: pointerB, base: 0, numRows: numRowsB, numColumns: numColumnsB};
      descriptorC <= MatrixDescriptor { pointer: pointerC, base: 0, numRows: numRowsA, numColumns: numRowsB};
      dpCount <= numRowsA*numRowsB;

      $display("mm pointerA=%d pointerB=%d pointerC=%d\n", pointerA, pointerB, pointerC);
      if (verbose) $display("mm.start ra=%d ca=%d rb=%d cb=%d dpCount=%d", numRowsA, numColumnsA, numRowsB, numColumnsB, dpCount);
      if (verbose) $display($format(fshow("mm.start ")+fshow(xycfg)));
      xypipeifc.start(xycfg);
   endmethod
   method ActionValue#(Bool) finish();
      $display("mm.finish()");
      doneFifo.deq();
      return True;
   endmethod

   interface ObjectWriteClient dmaClient = sinkC.dmaClient;
endmodule
