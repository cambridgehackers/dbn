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

import Vector::*;
import FIFO::*;
import FIFOF::*;
import DefaultValue::*;
import GetPut::*;
import ClientServer::*;
import Connectable::*;
import FloatingPoint::*;
import BRAM::*;

import AxiDma::*;
import PortalMemory::*;
import Dma::*;
import DmaVector::*;
import AxiMasterSlave::*;

import Matrix::*;
import Sigmoid::*;
import FloatOps::*;
import Pipe::*;

// these typedefs are so we can have synthesizeable modules below
typedef 1 N;
typedef TMul#(32,N) DmaSz;

interface RbmIndication;
   method Action elapsedCycles(Bit#(64) cycles, Bit#(64) idleCycles);
   method Action sigmoidDone();
   method Action sigmoidTableSize(Bit#(32) size);
   method Action sigmoidTableUpdated(Bit#(32) addr);
   method Action mmfDone();
   method Action bramMmfDone();
   method Action toBramDone();
   method Action fromBramDone();
   method Action statesDone();
   method Action statesDone2();
   method Action updateWeightsDone();
   method Action sumOfErrorSquared(Bit#(32) error);
   method Action dbg(Bit#(32) a, Bit#(32) b, Bit#(32) c, Bit#(32) d);
endinterface

interface RbmRequest;
   method Action sglist(Bit#(32) off, Bit#(40) addr, Bit#(32) len);
   method Action paref(Bit#(32) addr, Bit#(32) len);

   method Action startTimer();
   method Action stopTimer();
   method Action mmf(Bit#(32) inPointer1, Bit#(32) r1, Bit#(32) c1,
		     Bit#(32) inPointer2, Bit#(32) r2, Bit#(32) c2,
		     Bit#(32) outPointer);
   method Action bramMmf(Bit#(32) inPointer1, Bit#(32) r1, Bit#(32) c1,
			 Bit#(32) inPointer2, Bit#(32) r2, Bit#(32) c2,
			 Bit#(32) outPointer);
   method Action toBram(Bit#(32) off, Bit#(32) pointer, Bit#(32) offset, Bit#(32) numElts);
   method Action fromBram(Bit#(32) off, Bit#(32) pointer, Bit#(32) offset, Bit#(32) numElts);

    method Action sigmoid(Bit#(32) readPointer, Bit#(32) readOffset, Bit#(32) writePointer, Bit#(32) writeOffset, Bit#(32) numElts);
    method Action setSigmoidLimits(Bit#(32) rscale, Bit#(32) llimit, Bit#(32) ulimit);
    method Action sigmoidTableSize();
    method Action updateSigmoidTable(Bit#(32) readPointer, Bit#(32) readOffset, Bit#(32) numElts);

    method Action computeStates(Bit#(32) readPointer, Bit#(32) readOffset, Bit#(32) writePointer, Bit#(32) writeOffset, Bit#(32) numElts);
    method Action computeStates2(Bit#(32) readPointer, Bit#(32) readOffset, Bit#(32) readPointer2, Bit#(32) readOffset2,
				 Bit#(32) writePointer, Bit#(32) writeOffset, Bit#(32) numElts);

    method Action updateWeights(Bit#(32) posAssociationsPointer, Bit#(32) negAssociationsPointer, Bit#(32) weightsPointer, Bit#(32) numElts, Bit#(32) learningRateOverNumExamples);

    method Action sumOfErrorSquared(Bit#(32) dataPointer, Bit#(32) predPointer, Bit#(32) numElts);

    method Action dbg(); 
    method Action finish(); // for bsim only
endinterface

function ObjectReadClient#(asz) getSourceReadClient(DmaVectorSource#(asz,a) s); return s.dmaClient; endfunction
function ObjectWriteClient#(asz) getSinkWriteClient(DmaVectorSink#(asz,a) s); return s.dmaClient; endfunction

interface DramMatrixMultiply#(numeric type n, numeric type dmasz);
   interface Vector#(2, ObjectReadClient#(dmasz)) readClients;
   interface Vector#(1, ObjectWriteClient#(dmasz)) writeClients;
   interface PipeOut#(Bool) donePipe;
   method Action start(ObjectPointer pointerA, UInt#(ObjectOffsetSize) numRowsA, UInt#(ObjectOffsetSize) numColumnsA,
		       ObjectPointer pointerB, UInt#(ObjectOffsetSize) numRowsB, UInt#(ObjectOffsetSize) numColumnsB,
		       ObjectPointer pointerC);
   method Bit#(32) dbg();
endinterface

(* synthesize *)
module [Module] mkDramMatrixMultiply(DramMatrixMultiply#(N,TMul#(N,32)));
   Vector#(2, DmaVectorSource#(DmaSz, Vector#(N,Float))) vfsources <- replicateM(mkDmaVectorSource());
   Vector#(1, VectorSource#(DmaSz, Vector#(N,Float))) xvfsources = cons(vfsources[0].vector, nil);
   Vector#(1, VectorSource#(DmaSz, Vector#(N,Float))) yvfsources = cons(vfsources[1].vector, nil);
   DmaMatrixMultiplyIfc#(ObjectOffsetSize,DmaSz) dmaMMF <- mkDmaMatrixMultiply(xvfsources, yvfsources, mkDmaVectorSink);
   interface Vector readClients = map(getSourceReadClient, vfsources);
   interface Vector writeClients = cons(dmaMMF.dmaClient, nil);
   interface PipeOut donePipe = dmaMMF.pipe;
   method start = dmaMMF.start;
   method Bit#(32) dbg();
      Bit#(32) d = 0;
      d[0] = pack(vfsources[0].vector.pipe.notEmpty());
      d[1] = pack(vfsources[1].vector.pipe.notEmpty());
      return d;
   endmethod
endmodule

interface DramBramMatrixMultiply#(numeric type n, numeric type dmasz);
   interface Vector#(2, ObjectReadClient#(dmasz)) readClients;
   interface Vector#(2, ObjectWriteClient#(dmasz)) writeClients;
   interface PipeOut#(Bool) donePipe;
   method Action start(ObjectPointer pointerA, UInt#(ObjectOffsetSize) numRowsA, UInt#(ObjectOffsetSize) numColumnsA,
		       ObjectPointer pointerB, UInt#(ObjectOffsetSize) numRowsB, UInt#(ObjectOffsetSize) numColumnsB,
		       ObjectPointer pointerC);
   method Action toBram(Bit#(32) off, Bit#(32) pointer, Bit#(32) offset, Bit#(32) numElts);
   method ActionValue#(Bit#(32)) toBramDone();
   method Action fromBram(Bit#(32) off, Bit#(32) pointer, Bit#(32) offset, Bit#(32) numElts);
   method ActionValue#(Bit#(32)) fromBramDone();
   method Bit#(32) dbg();
endinterface

(* synthesize *)
module [Module] mkDramBramMatrixMultiply(DramBramMatrixMultiply#(N,TMul#(N,32)));
   Vector#(2, DmaVectorSource#(DmaSz, Vector#(N,Float))) vfsources <- replicateM(mkDmaVectorSource());
   Vector#(1, VectorSource#(DmaSz, Vector#(N,Float))) xvfsources = cons(vfsources[0].vector, nil);
   Vector#(1, VectorSource#(DmaSz, Vector#(N,Float))) yvfsources = cons(vfsources[1].vector, nil);

   BRAM_Configure bramCfg = defaultValue;
   bramCfg.memorySize = 4096;
   BRAM2Port#(Bit#(14), Vector#(N,Float)) bram2Server <- mkBRAM2Server(bramCfg);
   BramVectorSource#(14,32,Vector#(N,Float)) bramVectorSource <- mkBramVectorSource();
   mkConnection(bramVectorSource.bramClient, bram2Server.portA);

   FIFOF#(Vector#(N,Float)) wbfifo <- mkFIFOF();
   DmaVectorSink#(DmaSz, Vector#(N,Float)) writeBackVectorSink <- mkDmaVectorSink(toPipeOut(wbfifo));

   DmaMatrixMultiplyIfc#(ObjectOffsetSize,DmaSz) dmaMMF <- mkDmaMatrixMultiply(xvfsources, cons(bramVectorSource.vector, nil), mkDmaVectorSink);

   Reg#(Bit#(14)) bramWriteOffset <- mkReg(0);
   Reg#(Bit#(14)) bramWriteLimit <- mkReg(0);
   FIFO#(Bool) toBramDoneFifo <- mkFIFO();

   rule tobram if (bramWriteOffset < bramWriteLimit);
      let data = vfsources[1].vector.pipe.first();
      vfsources[1].vector.pipe.deq();

      bram2Server.portB.request.put( BRAMRequest { write: True, responseOnWrite: False, address: bramWriteOffset, datain: data });
      let offset = bramWriteOffset + 1;
      bramWriteOffset <= offset;
      if (offset == bramWriteLimit)
	 toBramDoneFifo.enq(True);
   endrule

   Reg#(Bit#(14)) bramReadOffset <- mkReg(0);
   Reg#(Bit#(14)) bramReadLimit <- mkReg(0);
   (* descending_urgency = "tobram, frombram" *)
   rule frombram if (bramReadOffset < bramReadLimit);
      bram2Server.portB.request.put( BRAMRequest { write: False, responseOnWrite: False, address: bramReadOffset, datain: ? });
      bramReadOffset <= bramReadOffset + 1;
   endrule
   rule bramtodram;
      let d <- bram2Server.portB.response.get();
      wbfifo.enq(d);
   endrule

   method Action toBram(Bit#(32) off, Bit#(32) pointer, Bit#(32) offset, Bit#(32) numElts) if (bramWriteOffset >= bramWriteLimit);
      bramWriteOffset <= truncate(off);
      bramWriteLimit <= truncate(off+numElts);
      vfsources[1].vector.start(pointer, extend(offset), extend(numElts));
   endmethod

   method ActionValue#(Bit#(32)) toBramDone();
      toBramDoneFifo.deq();
      vfsources[1].vector.finish();
      return 0;
   endmethod

   method Action fromBram(Bit#(32) off, Bit#(32) pointer, Bit#(32) offset, Bit#(32) numElts) if (bramReadOffset >= bramReadLimit);
      bramReadOffset <= truncate(off);
      bramReadLimit <= truncate(off+numElts);
      writeBackVectorSink.vector.start(pointer, extend(offset), extend(numElts));
   endmethod

   method ActionValue#(Bit#(32)) fromBramDone();
      let b <- writeBackVectorSink.vector.finish();
      return 0;
   endmethod

   interface Vector readClients = map(getSourceReadClient, vfsources);
   interface Vector writeClients = 
      cons(writeBackVectorSink.dmaClient,
	 cons(dmaMMF.dmaClient,
	    nil));
   interface PipeOut donePipe = dmaMMF.pipe;
   method start = dmaMMF.start;
   method Bit#(32) dbg();
      Bit#(32) d = 0;
      d[0] = pack(vfsources[0].vector.pipe.notEmpty());
      d[1] = pack(vfsources[1].vector.pipe.notEmpty());
      return d;
   endmethod
endmodule


interface DmaStatesPipe#(numeric type n, numeric type dmasz);
   interface Vector#(1, ObjectReadClient#(dmasz)) sources; 
   interface Vector#(1, ObjectWriteClient#(dmasz)) sinks; 
   method Action start(Bit#(32) readPointer, Bit#(32) readOffset,
		       Bit#(32) writePointer, Bit#(32) writeOffset, Bit#(32) numElts);
   method ActionValue#(Bool) finish();
endinterface

(* synthesize *)
module [Module] mkDmaStatesPipe(DmaStatesPipe#(N, DmaSz))
   provisos (Bits#(Vector#(N, Float), DmaSz)
	     );
   DmaVectorSource#(DmaSz, Vector#(N,Float)) statesource <- mkDmaVectorSource();
   PipeOut#(Vector#(N, Float)) dmaStatesPipe <- mkComputeStatesPipe(statesource.vector.pipe);
   DmaVectorSink#(DmaSz, Vector#(N, Float)) dmaStatesSink <- mkDmaVectorSink(dmaStatesPipe);

   interface Vector sources = cons(statesource.dmaClient, nil);
   interface Vector sinks = cons(dmaStatesSink.dmaClient, nil);
   method Action start(Bit#(32) readPointer, Bit#(32) readOffset,
				  Bit#(32) writePointer, Bit#(32) writeOffset, Bit#(32) numElts);
      statesource.vector.start(readPointer, extend(readOffset), extend(unpack(numElts)));
      dmaStatesSink.vector.start(writePointer, extend(writeOffset), extend(unpack(numElts)));
   endmethod
   method ActionValue#(Bool) finish();
      let b <- dmaStatesSink.vector.finish();
      return b;
   endmethod
endmodule

interface DmaStatesPipe2#(numeric type n, numeric type dmasz);
   interface Vector#(2, ObjectReadClient#(dmasz))  sources; 
   interface Vector#(1, ObjectWriteClient#(dmasz)) sinks; 
   method Action start(Bit#(32) readPointer, Bit#(32) readOffset,
		       Bit#(32) readPointer2, Bit#(32) readOffset2,
		       Bit#(32) writePointer, Bit#(32) writeOffset, Bit#(32) numElts);
   method ActionValue#(Bool) finish();
endinterface

(* synthesize *)
module [Module] mkDmaStatesPipe2(DmaStatesPipe2#(N, DmaSz))
   provisos (Bits#(Vector#(N, Float), DmaSz)
	     );
   Vector#(2, DmaVectorSource#(DmaSz, Vector#(N,Float))) statesources <- replicateM(mkDmaVectorSource());
   PipeOut#(Vector#(N, Float)) dmaStatesPipe <- mkComputeStatesPipe2(statesources[0].vector.pipe, statesources[1].vector.pipe);
   DmaVectorSink#(DmaSz, Vector#(N, Float)) dmaStatesSink <- mkDmaVectorSink(dmaStatesPipe);

   interface Vector sources = map(getSourceReadClient, statesources);
   interface Vector sinks = cons(dmaStatesSink.dmaClient, nil);
   method Action start(Bit#(32) readPointer, Bit#(32) readOffset,
		       Bit#(32) readPointer2, Bit#(32) readOffset2,
		       Bit#(32) writePointer, Bit#(32) writeOffset, Bit#(32) numElts);
      statesources[0].vector.start(readPointer, extend(readOffset), extend(unpack(numElts)));
      statesources[1].vector.start(readPointer2, extend(readOffset2), extend(unpack(numElts)));
      dmaStatesSink.vector.start(writePointer, extend(writeOffset), extend(unpack(numElts)));
   endmethod
   method ActionValue#(Bool) finish();
      let b <- dmaStatesSink.vector.finish();
      return b;
   endmethod
endmodule

interface DmaUpdateWeights#(numeric type n, numeric type dmasz);
   interface Vector#(3, ObjectReadClient#(dmasz)) readClients;
   interface Vector#(1, ObjectWriteClient#(dmasz)) writeClients;
   interface PipeOut#(Bool) donePipe;
   method Action start(Bit#(32) posAssociationsPointer, Bit#(32) negAssociationsPointer, Bit#(32) weightsPointer, Bit#(32) numElts, Float learningRateOverNumExamples);
   method ActionValue#(Bool) finish();
endinterface

(* synthesize *)
module [Module] mkDmaUpdateWeights(DmaUpdateWeights#(N, DmaSz))
   provisos (Bits#(Vector#(N, Float), DmaSz)
	     );
   Vector#(3, DmaVectorSource#(DmaSz, Vector#(N,Float))) sources <- replicateM(mkDmaVectorSource());

   let n = valueOf(N);

   Reg#(Float) learningRateOverNumExamples <- mkReg(defaultValue);

   FIFOF#(Vector#(N,Float)) wfifo <- mkFIFOF();
   Vector#(N, FloatServer2#(Float)) adders <- replicateM(mkFloatAdder());
   Vector#(N, FloatServer2#(Float)) adders2 <- replicateM(mkFloatAdder());
   Vector#(N, FloatServer2#(Float)) multipliers <- replicateM(mkFloatMultiplier());
   DmaVectorSink#(DmaSz, Vector#(N, Float)) sink <- mkDmaVectorSink(toPipeOut(wfifo));

// weights += learningRate * (pos_associations - neg_associations) / num_examples;
   rule sub;
      let pa = sources[0].vector.pipe.first();
      sources[0].vector.pipe.deq();
      let na = sources[1].vector.pipe.first();
      sources[1].vector.pipe.deq();
      for (Integer i = 0; i < n; i = i+1) begin
	 adders[i].request.put(tuple3(pa[i], -na[i], defaultValue));
      end
   endrule
   rule mul;
      for (Integer i = 0; i < n; i = i+1) begin
	 let sumexc <- adders[i].response.get();
	 multipliers[i].request.put(tuple3(learningRateOverNumExamples, tpl_1(sumexc), defaultValue));
      end
   endrule
   rule add;
      let weights = sources[2].vector.pipe.first();
      sources[2].vector.pipe.deq();
      for (Integer i = 0; i < n; i = i+1) begin
	 let resultexc <- multipliers[i].response.get();
	 adders2[i].request.put(tuple3(weights[i], tpl_1(resultexc), defaultValue));
      end
   endrule
   rule result;
      Vector#(N, Float) r;
      for (Integer i = 0; i < n; i = i+1) begin
	 let resultexc <- adders2[i].response.get();
	 r[i] = tpl_1(resultexc);
      end
      wfifo.enq(r);
   endrule

   for (Integer i = 0; i < 3; i = i + 1)
      rule finishSources;
	 let b <- sources[i].vector.finish();
      endrule

   //interface Vector sources = cons(statesource, nil);
   interface Vector readClients = map(getSourceReadClient, sources);
   interface Vector writeClients = cons(getSinkWriteClient(sink), nil);
   method Action start(Bit#(32) posAssociationsPointer, Bit#(32) negAssociationsPointer, Bit#(32) weightsPointer, Bit#(32) numElts, Float lrone);
      learningRateOverNumExamples <= lrone;
      sources[0].vector.start(posAssociationsPointer, 0, extend(numElts));
      sources[1].vector.start(negAssociationsPointer, 0, extend(numElts));
      sources[2].vector.start(weightsPointer, 0, extend(numElts));
      sink.vector.start(weightsPointer, 0, extend(numElts));
   endmethod
   method ActionValue#(Bool) finish();
      let b <- sink.vector.finish();
      return b;
   endmethod
endmodule
interface DmaSumOfErrorSquared#(numeric type n, numeric type dmasz);
   interface Vector#(2, ObjectReadClient#(dmasz)) readClients;
   interface PipeOut#(Float) pipe;
   method Action start(Bit#(32) dataPointer, Bit#(32) predPointer, Bit#(32) numElts);
endinterface

(* synthesize *)
module [Module] mkDmaSumOfErrorSquared(DmaSumOfErrorSquared#(N, DmaSz))
   provisos (Bits#(Vector#(N, Float), DmaSz)
	     );
   Vector#(2, DmaVectorSource#(DmaSz, Vector#(N,Float))) sources <- replicateM(mkDmaVectorSource());

   let n = valueOf(N);

   Reg#(Bit#(32)) resultCount <- mkReg(0);
   Vector#(N, FloatServer2#(Float)) adders <- replicateM(mkFloatAdder());
   Vector#(N, Server#(Tuple4#(Maybe#(Float), Float, Float, RoundMode), Tuple2#(Float,Exception))) macs <- replicateM(mkFloatMac());
   FIFOF#(Vector#(N, Float)) accfifo <- mkFIFOF();
   FIFOF#(Vector#(N, Float)) resultFifo <- mkFIFOF();

   // compute sum((data - pred)^2)

   rule sub;
      let data = sources[0].vector.pipe.first();
      sources[0].vector.pipe.deq();
      let pred = sources[1].vector.pipe.first();
      sources[1].vector.pipe.deq();
      for (Integer i = 0; i < n; i = i+1) begin
	 adders[i].request.put(tuple3(data[i], -pred[i], defaultValue));
      end
   endrule
   rule mac;
      let sum = accfifo.first;
      accfifo.deq;
      for (Integer i = 0; i < n; i = i+1) begin
	 let sumexc <- adders[i].response.get();
	 macs[i].request.put(tuple4(tagged Valid sum[i], tpl_1(sumexc), tpl_1(sumexc), defaultValue));
      end
   endrule
   rule acc if (resultCount > 0);
      Vector#(N, Float) sum = replicate(defaultValue);
      for (Integer i = 0; i < n; i = i+1) begin
	 let resultexc <- macs[i].response.get();
	 sum[i] = tpl_1(resultexc);
      end
      if (resultCount > 1)
	 accfifo.enq(sum);
      else
	 resultFifo.enq(sum);
      resultCount <= resultCount - 1;
   endrule

   PipeOut#(Vector#(N, Float)) sumPipe = toPipeOut(resultFifo);
   function Float fsum(Float a, Float b); return a+b; endfunction
   PipeOut#(Float) foldedPipe <- mkMap(foldl(fsum, fromReal(0.0)), sumPipe);

   for (Integer i = 0; i < 2; i = i + 1)
      rule finishSources;
	 let b <- sources[i].vector.finish();
      endrule

   //interface Vector sources = cons(statesource, nil);
   interface Vector readClients = map(getSourceReadClient, sources);
   interface PipeOut pipe = foldedPipe;
   method Action start(Bit#(32) dataPointer, Bit#(32) predPointer, Bit#(32) numElts) if (resultCount == 0);
      sources[0].vector.start(dataPointer, 0, extend(numElts));
      sources[1].vector.start(predPointer, 0, extend(numElts));
      Vector#(N, Float) sum = replicate(fromReal(0.0));
      accfifo.enq(sum);

      resultCount <= numElts;
   endmethod
endmodule

interface Rbm#(numeric type n);
   interface RbmRequest request;
   interface Vector#(12, ObjectReadClient#(TMul#(32,n))) readClients;
   interface Vector#(5, ObjectWriteClient#(TMul#(32,n))) writeClients;
endinterface

module [Module] mkRbm#(RbmIndication ind)(Rbm#(N))
   provisos (Add#(1,a__,N),
	     Add#(N,0,n),
	     Mul#(N,32,DmaSz)
      );

   let n = valueOf(n);

   DramMatrixMultiply#(N, TMul#(N,32)) dmaMMF <- mkDramMatrixMultiply();
   //DramBramMatrixMultiply#(N, TMul#(N,32)) bramMMF <- mkDramBramMatrixMultiply();

   Vector#(2, DmaVectorSource#(DmaSz, Vector#(N,Float))) sigmoidsources <- replicateM(mkDmaVectorSource());
   DmaSigmoidIfc#(TMul#(32,n)) dmaSigmoid <- mkDmaSigmoid(sigmoidsources[0].vector,
							  sigmoidsources[1].vector,
							  mkDmaVectorSink);

   DmaStatesPipe#(N, DmaSz) dmaStates <- mkDmaStatesPipe();
   DmaStatesPipe2#(N, DmaSz) dmaStates2 <- mkDmaStatesPipe2();

   DmaUpdateWeights#(N, DmaSz) dmaUpdateWeights <- mkDmaUpdateWeights();
   DmaSumOfErrorSquared#(N, DmaSz) dmaSumOfErrorSquared <- mkDmaSumOfErrorSquared();

   FIFOF#(Bool) busyFifo <- mkFIFOF();
   rule mmfDone;
      let d <- dmaMMF.donePipe.deq();
      busyFifo.deq();
      ind.mmfDone();
   endrule
   // rule bramMmfDone;
   //    let d <- bramMMF.donePipe.deq();
   //    busyFifo.deq();
   //    ind.bramMmfDone();
   // endrule
   // rule toBramDone;
   //    let d <- bramMMF.toBramDone();
   //    busyFifo.deq();
   //    ind.toBramDone();
   // endrule
   // rule fromBramDone;
   //    let d <- bramMMF.fromBramDone();
   //    busyFifo.deq();
   //    ind.fromBramDone();
   // endrule
   rule sigmoidDone;
      let d <- dmaSigmoid.finish();
      busyFifo.deq();
      ind.sigmoidDone();
   endrule
   rule sigmoidTableUpdateDone;
      let d <- dmaSigmoid.sigmoidTableUpdated();
      busyFifo.deq();
      ind.sigmoidTableUpdated(0);
   endrule
   rule dmaStatesDone;
      let b <- dmaStates.finish();
      busyFifo.deq();
      ind.statesDone();
   endrule
   rule dmaStatesDone2;
      let b <- dmaStates2.finish();
      busyFifo.deq();
      ind.statesDone2();
   endrule

   rule dmaUpdateWeightsDone;
      dmaUpdateWeights.donePipe.deq();
      busyFifo.deq();
      ind.updateWeightsDone();
   endrule

   rule dmaSumOfErrorSquaredDone;
      dmaSumOfErrorSquared.pipe.deq();
      busyFifo.deq();
      ind.sumOfErrorSquared(pack(dmaSumOfErrorSquared.pipe.first()));
   endrule

   FIFOF#(Bool) timerRunning <- mkFIFOF();
   Reg#(Bit#(64)) cycleCount <- mkReg(0);
   Reg#(Bit#(64)) idleCount <- mkReg(0);
   rule countCycles if (timerRunning.notEmpty());
      cycleCount <= cycleCount + 1;
      if (!busyFifo.notEmpty())
	 idleCount <= idleCount + 1;
   endrule

   interface RbmRequest request;
      method Action startTimer() if (!timerRunning.notEmpty());
	 cycleCount <= 0;
	 idleCount <= 0;
	 timerRunning.enq(True);
      endmethod
      method Action stopTimer();
	 timerRunning.deq();
	 ind.elapsedCycles(cycleCount, idleCount);
      endmethod

      method Action mmf(Bit#(32) h1, Bit#(32) r1, Bit#(32) c1,
			Bit#(32) h2, Bit#(32) r2, Bit#(32) c2,
			Bit#(32) h3);
	 dmaMMF.start(h1, unpack(extend(r1)), unpack(extend(c1)),
		      h2, unpack(extend(r2)), unpack(extend(c2)),
		      h3);
	 busyFifo.enq(True);
      endmethod
      method Action bramMmf(Bit#(32) h1, Bit#(32) r1, Bit#(32) c1,
			    Bit#(32) h2, Bit#(32) r2, Bit#(32) c2,
			    Bit#(32) h3);
	 // bramMMF.start(h1, unpack(extend(r1)), unpack(extend(c1)),
	 // 	       h2, unpack(extend(r2)), unpack(extend(c2)),
	 // 	       h3);
	 // busyFifo.enq(True);
      endmethod
      method Action toBram(Bit#(32) off, Bit#(32) pointer, Bit#(32) offset, Bit#(32) numElts);
	 // bramMMF.toBram(off, pointer, offset, numElts);
	 // busyFifo.enq(True);
      endmethod

      method Action fromBram(Bit#(32) off, Bit#(32) pointer, Bit#(32) offset, Bit#(32) numElts);
	 // bramMMF.fromBram(off, pointer, offset, numElts);
	 // busyFifo.enq(True);
      endmethod

      method Action sigmoid(Bit#(32) readPointer, Bit#(32) readOffset,
			    Bit#(32) writePointer, Bit#(32) writeOffset, Bit#(32) numElts);
	 dmaSigmoid.start(readPointer, writePointer, unpack(extend(numElts)));
	 busyFifo.enq(True);
      endmethod
      method Action setSigmoidLimits(Bit#(32) rscale, Bit#(32) llimit, Bit#(32) ulimit);
	 $display("rbm.setSigmoidLimits");
	 dmaSigmoid.setSigmoidLimits(unpack(rscale), unpack(llimit), unpack(ulimit));
	 busyFifo.enq(True);
      endmethod
      method Action updateSigmoidTable(Bit#(32) readPointer, Bit#(32) readOffset, Bit#(32) numElts);
	 $display("rbm.updateSigmoidTable pointer=%x addr=%h numElts=%d", readPointer, readOffset, numElts);
	 dmaSigmoid.updateSigmoidTable(readPointer, extend(readOffset), extend(numElts));
	 busyFifo.enq(True);
      endmethod
      method Action sigmoidTableSize();
	 ind.sigmoidTableSize(dmaSigmoid.tableSize());
      endmethod

      method Action dbg();
      endmethod

      method Action finish();
	 $finish(0);
      endmethod
      method Action computeStates(Bit#(32) readPointer, Bit#(32) readOffset,
				  Bit#(32) writePointer, Bit#(32) writeOffset, Bit#(32) numElts);
	 //$display("computeStates rh=%d wh=%d len=%d", readPointer, writePointer, numElts);
      
	 busyFifo.enq(True);
      endmethod
      method Action computeStates2(Bit#(32) readPointer, Bit#(32) readOffset,
				   Bit#(32) readPointer2, Bit#(32) readOffset2,
				   Bit#(32) writePointer, Bit#(32) writeOffset, Bit#(32) numElts);
	 //$display("computeStates2 rh=%d wh=%d len=%d", readPointer, writePointer, numElts);
	 dmaStates2.start(readPointer, readOffset,
			  readPointer2, readOffset2,
			  writePointer,writeOffset, numElts);

	 busyFifo.enq(True);
      endmethod
      method Action updateWeights(Bit#(32) posAssociationsPointer,
	 Bit#(32) negAssociationsPointer,
				  Bit#(32) weightsPointer,
				  Bit#(32) numElts,
				  Bit#(32) learningRateOverNumExamples);
	 dmaUpdateWeights.start(posAssociationsPointer, negAssociationsPointer, weightsPointer,
				numElts, unpack(learningRateOverNumExamples));
	 busyFifo.enq(True);
      endmethod
      method Action sumOfErrorSquared(Bit#(32) dataPointer, Bit#(32) predPointer, Bit#(32) numElts);
	 dmaSumOfErrorSquared.start(dataPointer, predPointer, numElts);
	 busyFifo.enq(True);
      endmethod
   endinterface   

   interface Vector readClients = append(
					 //append(
					    dmaMMF.readClients,
					    //bramMMF.readClients
					    //),
					 append(
						append(dmaUpdateWeights.readClients,
						       dmaSumOfErrorSquared.readClients),
						append(
						   map(getSourceReadClient, sigmoidsources),
						   append(
						      dmaStates.sources,
						      dmaStates2.sources)
						   )));

   interface Vector writeClients = append(
      append(dmaUpdateWeights.writeClients,
	     cons(dmaSigmoid.dmaClient, cons(dmaStates.sinks[0], cons(dmaStates2.sinks[0], nil)))),
//      append(
	 dmaMMF.writeClients //,	     bramMMF.writeClients)
      );
endmodule
