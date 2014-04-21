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

import FIFOF::*;
import GetPut::*;
import Connectable::*;
import Vector::*;
import MIMO::*;
import DefaultValue::*;

interface PipeOut#(type a);
   method a first();
   method Action deq();
   method Bool notEmpty();
endinterface

typeclass PipeInOut#(type a, type b);
   function PipeOut#(a) toPipeOut(b in);
endtypeclass

typeclass MkPipeInOut#(type a, type b);
   module mkPipeOut#(b in)(PipeOut#(a));
endtypeclass

instance PipeInOut#(a, FIFOF#(a));
   function PipeOut#(a) toPipeOut(FIFOF#(a) in);
      return (interface PipeOut#(a);
		 method first = in.first;
		 method deq = in.deq;
		 method notEmpty = in.notEmpty;
	      endinterface);
   endfunction
endinstance

instance MkPipeInOut#(a, Get#(a))
   provisos (Bits#(a, asz));
   module mkPipeOut#(Get#(a) in)(PipeOut#(a));
      FIFOF#(a) fifo <- mkFIFOF();
      rule connect;
	 let v <- in.get();
	 fifo.enq(v);
      endrule
      return toPipeOut(fifo);
   endmodule
endinstance

instance Connectable#(PipeOut#(a),Put#(a));
   module mkConnection#(PipeOut#(a) in, Put#(a) out)(Empty);
      rule connect;
	 let v = in.first;
	 in.deq();
	 out.put(v);
      endrule
   endmodule
endinstance

module mkUnfunnel#(PipeOut#(Vector#(m,a)) in)(PipeOut#(Vector#(mk, a)))
   provisos (Mul#(m, k, mk),
	     Bits#(a, asz),
	     Add#(1, b__, asz),
	     Add#(2, c__, mk),
	     Add#(d__, m, mk),
	     Add#(asz, m, e__),
	     Add#(asz, mk, f__));
   let m = fromInteger(valueOf(m));
   let mk = fromInteger(valueOf(mk));

   MIMOConfiguration cfg = defaultValue();
   MIMO#(m, mk, mk, a) mimo <- mkMIMO(cfg);
   rule consumer if (mimo.enqReadyN(m));
      Vector#(m, a) v = in.first();
      in.deq();
      mimo.enq(m, v);
   endrule

   method Vector#(mk, a) first() if (mimo.deqReadyN(mk));
      return mimo.first();
   endmethod
   method Action deq() if (mimo.deqReadyN(mk));
      mimo.deq(mk);
   endmethod
   method notEmpty();
      return mimo.deqReadyN(mk);
   endmethod
endmodule

module mkForkVector#(PipeOut#(a) inpipe)(Vector#(n, PipeOut#(a)))
   provisos (Bits#(a, asz));
   Vector#(n, FIFOF#(a)) fifos <- replicateM(mkFIFOF());
   rule forkelts;
      let v = inpipe.first();
      inpipe.deq;
      for (Integer i = 0; i < valueOf(n); i = i + 1) begin
	 fifos[i].enq(v);
      end
   endrule
   return map(toPipeOut, fifos);
endmodule

module mkJoin#(function c f(a av, b bv), PipeOut#(a) apipe, PipeOut#(b) bpipe)(PipeOut#(c));
   method c first();
      let av = apipe.first();
      let bv = bpipe.first();
      return f(av, bv);
   endmethod
   method Action deq();
      apipe.deq();
      bpipe.deq();
   endmethod
   method Bool notEmpty();
      return apipe.notEmpty() && bpipe.notEmpty();
   endmethod
endmodule

module mkMap#(function b f(a av), PipeOut#(a) apipe)(PipeOut#(b));
   method b first();
      let av = apipe.first();
      return f(av);
   endmethod
   method Action deq();
      apipe.deq();
   endmethod
   method Bool notEmpty();
      return apipe.notEmpty();
   endmethod
endmodule

typedef (function Module #(PipeOut #(tb)) mkPipeOut(PipeOut#(ta) ifc)) MkPipeOut#(type ta, type tb);

typeclass ReducePipe#( numeric type n, type a);
   module [Module] mkReducePipe (MkPipeOut#(Tuple2#(a,a), a) combinepipe,
				 PipeOut#(Vector#(n,a)) inpipe,
				 PipeOut#(a) ifc);
endtypeclass
instance ReducePipe#(1, a);
   module [Module] mkReducePipe (MkPipeOut#(Tuple2#(a,a), a) combinepipe,
				 PipeOut#(Vector#(1,a)) inpipe,
				 PipeOut#(a) ifc);
      let pipe <- mkMap(head, inpipe);
      return pipe;
   endmodule
endinstance
instance ReducePipe#(2, a);
   module [Module] mkReducePipe (MkPipeOut#(Tuple2#(a,a), a) combinepipe,
				 PipeOut#(Vector#(2,a)) inpipe,
				 PipeOut#(a) ifc);
      function Tuple2#(a,a) foo(Vector#(2,a) invec); return tuple2(invec[0], invec[1]); endfunction
      PipeOut#(Tuple2#(a,a)) zippipe <- mkMap(foo, inpipe);
      let pipe <- combinepipe(zippipe);
      return pipe;
   endmodule
endinstance

instance ReducePipe#(n, a)
   provisos (Add#(TDiv#(n,2), a__, n),
	     Bits#(Vector#(TDiv#(n,2), a), b__),
	     ReducePipe#(TDiv#(n,2),a));
   module [Module] mkReducePipe (MkPipeOut#(Tuple2#(a,a), a) combinepipe,
				 PipeOut#(Vector#(n,a)) inpipe,
				 PipeOut#(a) ifc);
      FIFOF#(Vector#(TDiv#(n,2),a)) infifo0 <- mkFIFOF;
      FIFOF#(Vector#(TDiv#(n,2),a)) infifo1 <- mkFIFOF;
      rule splitinput;
	 let v = inpipe.first();
	 inpipe.deq();
	 infifo0.enq(takeAt(0, v));
	 infifo1.enq(takeAt(valueOf(TDiv#(n,2)), v));
      endrule
      PipeOut#(Vector#(TDiv#(n,2),a)) inpipe0 = toPipeOut(infifo0);
      PipeOut#(Vector#(TDiv#(n,2),a)) inpipe1 = toPipeOut(infifo1);
   
      PipeOut#(a) p0 <- mkReducePipe(combinepipe, inpipe0);
      PipeOut#(a) p1 <- mkReducePipe(combinepipe, inpipe1);

      PipeOut#(Tuple2#(a,a)) tplpipe = (interface PipeOut;
					method Tuple2#(a,a) first(); return tuple2(p0.first, p1.first); endmethod
					method Action deq(); p0.deq(); p1.deq(); endmethod
					method notEmpty(); return p0.notEmpty() && p1.notEmpty(); endmethod
					endinterface);
      PipeOut#(a) outpipe <- combinepipe(tplpipe);
      return outpipe;
   endmodule
endinstance
