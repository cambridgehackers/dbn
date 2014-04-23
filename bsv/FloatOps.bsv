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

typedef Server#(Tuple3#(a,a,RoundMode), Tuple2#(a,Exception)) FloatServer2#(type a);


(* synthesize *)
module mkFloatAdder(FloatServer2#(Float));
   let adder <- mkFloatingPointAdder();
   interface Put request;
      method Action put(Tuple3#(Float,Float,RoundMode) req);
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

module mkFloatAddPipe#(PipeOut#(Tuple2#(Float,Float)) xypipe)(PipeOut#(Float));
   let adder <- mkFloatAdder();
   FIFOF#(Float) fifo <- mkFIFOF();
   rule consumexy;
      let xy = xypipe.first();
      xypipe.deq;
      adder.request.put(tuple3(tpl_1(xy),tpl_2(xy),defaultValue));
   endrule
   rule enqout;
      let resp <- adder.response.get();
      fifo.enq(tpl_1(resp));
   endrule
   return toPipeOut(fifo);
endmodule

(* synthesize *)
module mkFloatMultiplier(FloatServer2#(Float));
   let multiplier <- mkFloatingPointMultiplier();
   interface Put request;
      method Action put(Tuple3#(Float,Float,RoundMode) req);
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

(* synthesize *)
module mkFloatMac(Server#(Tuple4#(Maybe#(Float), Float, Float, RoundMode), Tuple2#(Float,Exception)));
   let mac <- mkFpMac();
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
