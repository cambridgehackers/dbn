import RegFile::*;
import FIFO::*;
import FIFOF::*;
import Vector::*;
import FIFOF::*;
import Connectable::*;
import ClientServer::*;
import Memory::*;
import BRAM::*;
import DefaultValue::*;
import FloatingPoint::*;
import Real::*;
import BRAM::*;
import Dma::*;
import DmaVector::*;
import Pipe::*;
import FloatOps::*;

interface SigmoidTable#(numeric type tsz);
   interface Vector#(2, BRAMServer#(Bit#(tsz), Vector#(3,Float))) ports;
   interface ReadOnly#(Float) rscale;
   interface ReadOnly#(Float) llimit;
   interface ReadOnly#(Float) ulimit;
   method Action setSigmoidLimits(Float rscale, Float llimit, Float ulimit);
   method Action updateSigmoidTable(Bit#(tsz) addr, Vector#(3,Float) v);
   method Bit#(32) tableSize();
endinterface

module mkSigmoidTable(SigmoidTable#(tsz));
   let tsz = valueOf(tsz);
   BRAM_Configure bramCfg = defaultValue;
   let memorySize = 2**tsz;
   bramCfg.memorySize = memorySize;
   BRAM2Port#(Bit#(tsz), Vector#(3, Float)) sigmoidTable <- mkBRAM2Server(bramCfg);

   Reg#(Float) rscaleReg <- mkReg(fromReal(0.0));
   Reg#(Float) llimitReg <- mkReg(fromReal(0.0));
   Reg#(Float) ulimitReg <- mkReg(fromReal(0.0));

   Vector#(2, BRAMServer#(Bit#(tsz), Vector#(3,Float))) bramPorts;
   bramPorts[0] = sigmoidTable.portA;
   bramPorts[1] = sigmoidTable.portB;

   interface ReadOnly rscale = regToReadOnly(rscaleReg);
   interface ReadOnly llimit = regToReadOnly(llimitReg);
   interface ReadOnly ulimit = regToReadOnly(ulimitReg);
   interface Vector ports = bramPorts;
   method Action setSigmoidLimits(Float _rscale, Float _llimit, Float _ulimit);
      $display("memorySize=%d\n", memorySize);
      rscaleReg <= _rscale;
      llimitReg <= _llimit;
      ulimitReg <= _ulimit;
      $display($format("rscale=", fshow(_rscale), "pack(rscale)=", fshow(pack(_rscale)), " llimit=", fshow(_llimit), " ulimit=", fshow(_ulimit)));
   endmethod
   method Action updateSigmoidTable(Bit#(tsz) addr, Vector#(3,Float) v);
      sigmoidTable.portB.request.put(BRAMRequest{ write: True, responseOnWrite: False, address: addr, datain: v});
   endmethod
   method Bit#(32) tableSize();
      return (1 << tsz);
   endmethod
endmodule

module mkSigmoidServer#(SigmoidTable#(tsz) sigmoidTable)(Server#(Float,Float))
   provisos (Add#(tsz,2,usz),
	     Add#(a__, usz, 32)
	     );
   let tsz = valueOf(tsz);
   Int#(usz) numEntries = 1<<fromInteger(tsz);
   // linear approximation around in range [-8,8]

   Bool verbose = False;
   FIFOF#(Float) angleFifo2 <- mkFIFOF();
   FIFOF#(Float) angleFifo <- mkFIFOF();
   let multiplier <- mkFloatMultiplier();
   let adder <- mkFloatAdder();
   let mac <- mkFloatMac();

   rule lookupEntry;
      Float angle = 0;
      if (verbose) begin
	 angle = angleFifo2.first;
	 angleFifo2.deq();
      end
      let response <- multiplier.response.get();
      Float scaled_angle = tpl_1(response);
      Exception e = tpl_2(response);
      if (pack(e) != 0) $display("lookup.exception e=%h scaled_angle=%h", e, pack(scaled_angle));
      
      Int#(usz) i = truncate(toInt32(scaled_angle));
      Bit#(tsz) index = truncate(pack(i + numEntries/2));
      if (i > (numEntries/2-1))
	 index = truncate(pack(numEntries-1));
      if (i < -numEntries/2)
	 index = 0;
      if (verbose)
	 $display("sigmoid angle=%h i=%d index=%d numEntries=%d", pack(angle), i, index, numEntries);
      sigmoidTable.ports[0].request.put(BRAMRequest{ write: False, responseOnWrite: False, address: index, datain: ?});
   endrule

   FIFOF#(Vector#(3, Float)) vFifo <- mkFIFOF();
   rule computeDelta;
      let vs <- sigmoidTable.ports[0].response.get();
      let angle = angleFifo.first();
      angleFifo.deq();
      if (verbose) $display("angle=%h vs[0]=%h", pack(angle), pack(vs[0]));
      adder.request.put(tuple3(angle, vs[0], defaultValue));
      vFifo.enq(vs);
   endrule

   rule interpolate;
      // delta = angle + vs[0]
      // return vs[1] + vs[2]*delta
      let vs = vFifo.first();
      vFifo.deq();
      let result <- adder.response.get();
      let delta = tpl_1(result);
      Exception e = tpl_2(result);
      if (pack(e) != 0) $display("interpolate.exception e=%h delta=%h", e, pack(delta));
      if (verbose) $display("delta=%h vs[1]=%h vs[2]", pack(delta), pack(vs[1]), pack(vs[2]));
      mac.request.put(tuple4(tagged Valid vs[1], vs[2], delta, defaultValue));
   endrule

   interface Put request;
      method Action put(Float angle);
	 let c = compareFP(angle, sigmoidTable.llimit);
	 let d = compareFP(angle, sigmoidTable.ulimit);
	 if (c == LT)
	    angle = sigmoidTable.llimit;
	 if (d == GT)
	    angle = sigmoidTable.ulimit;
	 if (verbose)
	    angleFifo2.enq(angle);
	 angleFifo.enq(angle);
	 multiplier.request.put(tuple3(angle, sigmoidTable.rscale, defaultValue));
      endmethod
   endinterface
   interface Get response;
      method ActionValue#(Float) get();
	 let result <- mac.response.get();
	 let v = tpl_1(result);
	 Exception e = tpl_2(result);
	 if (pack(e) != 0) $display("mac.exception e=%h v=%h", e, pack(v));
	return v;
     endmethod
   endinterface
endmodule

module [Module] mkSigmoid#(SigmoidTable#(tsz) sigmoidTable, PipeOut#(Float) in)(PipeOut#(Float))
   provisos (Add#(a__, TAdd#(tsz, 2), 32));

   let server <- mkSigmoidServer(sigmoidTable);

   FIFOF#(Float) rfifo <- mkFIFOF;
   rule sigmoidOut;
      let sv <- server.response.get();
      rfifo.enq(sv);
   endrule
   mkConnection(in, server.request);
   return toPipeOut(rfifo);
endmodule

interface DmaSigmoidIfc#(numeric type dsz);
   method Action start(DmaPointer pointerA, DmaPointer pointerB, UInt#(DmaOffsetSize) numElts);
   method Action setSigmoidLimits(Float rscale, Float llimit, Float ulimit);
   method Action updateSigmoidTable(Bit#(32) readPointer, Bit#(32) readOffset, Bit#(32) numElts);
   method Bit#(32) tableSize();
   interface DmaWriteClient#(dsz) dmaClient;
   interface PipeOut#(Bool) pipe;
   interface PipeOut#(Bool) sigmoidTablePipe;
endinterface

module [Module] mkDmaSigmoid#(VectorSource#(dmasz, Vector#(n,Float)) source,
			      function Module#(DmaVectorSink#(dsz, Vector#(n, Float))) mkSink(PipeOut#(Vector#(n, Float)) pipe_in)
   )
   (DmaSigmoidIfc#(dsz))
   provisos (Bits#(Float, fsz)
	     , Mul#(fsz,n,dmasz)
	     , Add#(1, a__, n)
	     , Add#(b__, n, 4)
	     , Mul#(n, c__, 4)
      );

   Vector#(n, SigmoidTable#(6)) sigmoidTables <- replicateM(mkSigmoidTable);
   Vector#(n, Server#(Float,Float)) sigmoidServers;
   for (Integer i = 0; i < valueOf(n); i = i + 1) begin
      let s <- mkSigmoidServer(sigmoidTables[i]);
      sigmoidServers[i] = s;
   end

   Reg#(Bool) updatingSigmoidTable <- mkReg(False);
   Reg#(Bit#(DmaOffsetSize)) numElts <- mkReg(0);
   Reg#(Bit#(6)) entryNumber <- mkReg(0);

   FIFOF#(Bool) sigmoidUpdatedFifo <- mkFIFOF();

   PipeOut#(Vector#(4, Float)) sigmoidSourceFunnel <- mkUnfunnel(source.pipe);
   rule updateSigmoidTableRule if (updatingSigmoidTable);
      let vs = sigmoidSourceFunnel.first;
      sigmoidSourceFunnel.deq();
      Vector#(3,Float) vs3 = take(vs);

      for (Integer i = 0; i < valueOf(n); i = i + 1) begin
	 sigmoidTables[i].updateSigmoidTable(entryNumber, vs3);
      end
      if (numElts == 1) begin
	 updatingSigmoidTable <= False;
	 sigmoidUpdatedFifo.enq(True);
      end
      numElts <= numElts - 1;
      entryNumber <= entryNumber + 1;
   endrule

   rule consumeInput if (!updatingSigmoidTable);
      let vs = source.pipe.first;
      source.pipe.deq();
      for (Integer i = 0; i < valueOf(n); i = i + 1) begin
         sigmoidServers[i].request.put(vs[i]);
      end
   endrule

   FIFOF#(Vector#(n, Float)) dfifo <- mkFIFOF();
   rule enqResult;
      Vector#(n, Float) vs;
      for (Integer i = 0; i < valueOf(n); i = i + 1) begin
         let v <- sigmoidServers[i].response.get();
	 vs[i] = v;
      end
      dfifo.enq(vs);
   endrule

   let sinkC <- mkSink(toPipeOut(dfifo));

   method Action start(DmaPointer pointerA, DmaPointer pointerB, UInt#(DmaOffsetSize) count);
      source.start(pointerA, 0, pack(count));
      sinkC.vector.start(pointerB, 0, pack(count));
      //$display("sigmoid.start numElts=%d", numElts);
   endmethod
   method Action setSigmoidLimits(Float rscale, Float llimit, Float ulimit);
      for (Integer i = 0; i < valueOf(n); i = i + 1) begin
	 sigmoidTables[i].setSigmoidLimits(rscale, llimit, ulimit);
      end
   endmethod
   method Action updateSigmoidTable(Bit#(32) readPointer, Bit#(32) readOffset, Bit#(32) count);
      entryNumber <= 0;
      numElts <= extend(count);
      updatingSigmoidTable <= True;
      source.start(readPointer, extend(readOffset), 4*extend(count));
   endmethod
   method Bit#(32) tableSize();
      return sigmoidTables[0].tableSize();
   endmethod

   interface DmaWriteClient dmaClient = sinkC.dmaClient;
   interface PipeOut pipe = sinkC.vector.pipe;
   interface PipeOut sigmoidTablePipe = toPipeOut(sigmoidUpdatedFifo);

endmodule
