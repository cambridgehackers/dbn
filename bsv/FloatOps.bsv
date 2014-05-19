import FIFOF::*;
import GetPut::*;
import ClientServer::*;
import FloatingPoint::*;
import DefaultValue::*;
import Randomizable::*;
import Vector::*;
import StmtFSM::*;
import Pipe::*;
import FIFO::*;
import FpMac::*;

`ifdef BSIM
//`define SINGLE_CYCLE_ALU
`endif

`ifdef SINGLE_CYCLE_ALU
typedef 1 FP_ADD_DEPTH;
typedef 1 FP_MUL_DEPTH;
`else
typedef 5 FP_ADD_DEPTH;
typedef 5 FP_MUL_DEPTH;
`endif

interface FloatAlu#(numeric type n);
   interface Put#(Tuple2#(Float,Float)) request;
   interface Get#(Tuple2#(Float,Exception)) response;
endinterface

(* synthesize *)
`ifdef SINGLE_CYCLE_ALU
module mkFloatAdder#(RoundMode rmode)(FloatAlu#(FP_ADD_DEPTH));
   let foo <- mkSizedFIFO(valueOf(FP_ADD_DEPTH)+1);
   interface Put request;
      method Action put(Tuple2#(Float,Float) req);
	 match { .a, .b } = req;
	 foo.enq(a+b);
      endmethod
   endinterface
   interface Get response;
      method ActionValue#(Tuple2#(Float,Exception)) get();
	 let resp <- toGet(foo).get;
	 return tuple2(resp,defaultValue);
      endmethod
   endinterface
endmodule
`else
module mkFloatAdder#(RoundMode rmode)(FloatAlu#(FP_ADD_DEPTH));
`ifdef BSIM
   let adder <- mkFPAdder(rmode);
`else
   let adder <- mkXilinxFPAdder(rmode);
`endif
   interface Put request;
      method Action put(Tuple2#(Float,Float) req);
	 match { .a, .b } = req;
	 let tpl3 = tuple3(a, b, rmode);
         adder.request.put(req);
      endmethod
   endinterface
   interface Get response;
      method ActionValue#(Tuple2#(Float,Exception)) get();
	 let resp <- adder.response.get();
	 return resp;
      endmethod
   endinterface
endmodule
`endif

module mkFloatAddPipe#(PipeOut#(Tuple2#(Float,Float)) xypipe)(PipeOut#(Float));
   let adder <- mkFloatAdder(defaultValue);
   FIFOF#(Float) fifo <- mkFIFOF();
   rule consumexy;
      let xy = xypipe.first();
      xypipe.deq;
      adder.request.put(tuple2(tpl_1(xy),tpl_2(xy)));
   endrule
   rule enqout;
      let resp <- adder.response.get();
      fifo.enq(tpl_1(resp));
   endrule
   return toPipeOut(fifo);
endmodule

(* synthesize *)
`ifdef SINGLE_CYCLE_ALU
module mkFloatMultiplier#(RoundMode rmode)(FloatAlu#(FP_MUL_DEPTH));
   let foo <- mkSizedFIFO(valueOf(FP_MUL_DEPTH)+1);
   interface Put request;
      method Action put(Tuple2#(Float,Float) req);
	 foo.enq(tpl_1(req)*tpl_2(req));
      endmethod
   endinterface
   interface Get response;
      method ActionValue#(Tuple2#(Float,Exception)) get();
	 let resp <- toGet(foo).get;
	 return tuple2(resp,defaultValue);
      endmethod
   endinterface
endmodule
`else
module mkFloatMultiplier#(RoundMode rmode)(FloatAlu#(FP_MUL_DEPTH));
`ifdef BSIM
   let multiplier <- mkFPMultiplier(rmode);
`else
   let multiplier <- mkXilinxFPMultiplier(rmode);
`endif
   interface Put request;
      method Action put(Tuple2#(Float,Float) req);
         multiplier.request.put(req);
      endmethod
   endinterface
   interface Get response;
      method ActionValue#(Tuple2#(Float,Exception)) get();
	 let resp <- multiplier.response.get();
	 return resp;
      endmethod
   endinterface
endmodule
`endif

(* synthesize *)
module mkFloatMac#(RoundMode rmode) (Server#(Tuple3#(Maybe#(Float), Float, Float), Tuple2#(Float,Exception)));
   let mac <- mkFpMac(rmode);
   return mac;
endmodule

(* synthesize *)
module mkRandomPipe(PipeOut#(Float));
   let randomizer <- mkConstrainedRandomizer(0, 1024);

   Reg#(Bool) initted <- mkReg(False);
   rule first if (!initted);
      randomizer.cntrl.init();
      initted <= True;
   endrule

   let pipe_out <- mkPipeOut(interface Get#(Float);
				method ActionValue#(Float) get();
				   let v <- randomizer.next();
				   Float f = fromInt32(v); 
				   return f;
				endmethod
			     endinterface);
   return pipe_out;
endmodule

module [Module] mkComputeStatesPipe#(PipeOut#(Vector#(n, Float)) pipe_in)(PipeOut#(Vector#(n, Float)))
   provisos(Add#(a__, 1, n)
      );
   FIFOF#(Float) rfifo <- mkFIFOF();
   let randomPipe <- mkRandomPipe();
//   PipeOut#(Vector#(1, Float)) v1randomPipe <- mkFn_to_Pipe(replicate, randomPipe);
//   PipeOut#(Vector#(n, Float)) vrandomPipe <- mkUnfunnel(True, v1randomPipe);

   function Float greater(Float a, Float b);
      if (compareFP(a, b) == GT)
	 return 1.0;
      else
	 return 0.0;
   endfunction
   function Vector#(n, Float) vgreater(Vector#(n, Float) x, Vector#(n, Float) y);
      return map(uncurry(greater), zip(x, y));
   endfunction
//   let joinPipe <- mkJoin(vgreater, pipe_in, vrandomPipe);
   FIFOF#(Vector#(n, Float)) dfifo <- mkFIFOF();
   rule rfoo;
      let r = randomPipe.first();
      randomPipe.deq();
      //$display("random = %x", pack(r));
      rfifo.enq(r);
   endrule
   rule foo;
//      let v = joinPipe.first();
//      joinPipe.deq();
      let vs = pipe_in.first();
      pipe_in.deq();
      let r = rfifo.first();
      rfifo.deq();
      // FIXME: hack!
      Vector#(n, Float) rs = replicate(r);

      let gs = vgreater(vs, rs); 

      //$display($format(fshow("vs=")+fshow(vs) + fshow(" rs=") + fshow(rs) + fshow(" states=") + fshow(gs)));
      dfifo.enq(gs);
   endrule

   return toPipeOut(dfifo);
endmodule

module [Module] mkComputeStatesPipe2#(PipeOut#(Vector#(n, Float)) pipe_in, PipeOut#(Vector#(n, Float)) randomPipe)(PipeOut#(Vector#(n, Float)))
   provisos(Add#(a__, 1, n)
      );

   function Float greater(Float a, Float b);
      if (compareFP(a, b) == GT)
	 return 1.0;
      else
	 return 0.0;
   endfunction
   function Vector#(n, Float) vgreater(Vector#(n, Float) x, Vector#(n, Float) y);
      return map(uncurry(greater), zip(x, y));
   endfunction
//   let joinPipe <- mkJoin(vgreater, pipe_in, vrandomPipe);
   FIFOF#(Vector#(n, Float)) dfifo <- mkFIFOF();
   rule foo;
//      let v = joinPipe.first();
//      joinPipe.deq();
      let vs = pipe_in.first();
      pipe_in.deq();
      let rs = randomPipe.first();
      randomPipe.deq();

      let gs = vgreater(vs, rs); 

      //$display($format(fshow("vs=")+fshow(pack(vs)) + fshow(" rs=") + fshow(pack(rs)) + fshow(" states=") + fshow(gs)));
      dfifo.enq(gs);
   endrule

   return toPipeOut(dfifo);
endmodule
