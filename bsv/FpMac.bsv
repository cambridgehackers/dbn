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
import BUtils::*;


////////////////////////////////////////////////////////////////////////////////
/// Floating point fused multiple accumulate
////////////////////////////////////////////////////////////////////////////////
///
/// copied from FloatingPoint.bsv and modified.  
/// this version is no longer IEEE compliant :)
///
////////////////////////////////////////////////////////////////////////////////

typedef struct {
   Maybe#(FloatingPoint#(e,m)) res;
   Exception exc;
   RoundMode rmode;
   } CommonState#(numeric type e, numeric type m) deriving (Bits, Eq);

function Bit#(e) unbias( FloatingPoint#(e,m) din );
   return (din.exp - fromInteger(bias(din)));
endfunction

function Bit#(m) zExtendLSB(Bit#(n) value)
   provisos( Add#(n,m,k) );
   Bit#(k) out = { value, 0 };
   return out[valueof(k)-1:valueof(n)];
endfunction

function Integer minexp( FloatingPoint#(e,m) din );
  return 1-bias(din);
endfunction

function Bit#(1) getHiddenBit( FloatingPoint#(e,m) din );
   return (isSubNormal(din)) ? 0 : 1;
endfunction

function Integer bias( FloatingPoint#(e,m) din );
   return (2 ** (valueof(e)-1)) - 1;
endfunction

function Integer minexp_subnormal( FloatingPoint#(e,m) din );
   return minexp(din)-valueof(m);
endfunction

function Integer maxexp( FloatingPoint#(e,m) din );
   return bias(din);
endfunction

function Tuple2#(FloatingPoint#(e,m),Exception) round( RoundMode rmode, FloatingPoint#(e,m) din, Bit#(2) guard )
   provisos(  Add#(m, 1, m1)
	    , Add#(m, 2, m2)
	    );

   FloatingPoint#(e,m) out = defaultValue;
   Exception exc = defaultValue;

   if (isNaNOrInfinity(din)) begin
      out = din;
   end
   else begin
      let din_inc = din;

      Bit#(TAdd#(m,2)) sfd = unpack({1'b0, getHiddenBit(din), din.sfd}) + 1;

      if (msb(sfd) == 1) begin
	 if (din.exp == fromInteger(maxexp(din) + bias(out))) begin
	    din_inc = infinity(din_inc.sign);
	 end
	 else begin
	    din_inc.exp = din_inc.exp + 1;
	    din_inc.sfd = truncate(sfd >> 1);
	 end
      end
      else if ((din.exp == 0) && (truncateLSB(sfd) == 2'b01)) begin
	 din_inc.exp = 1;
	 din_inc.sfd = truncate(sfd);
      end
      else begin
	 din_inc.sfd = truncate(sfd);
      end

      if (guard != 0) begin
	 exc.inexact = True;
      end

      case(rmode)
	 Rnd_Nearest_Even:
	 begin
	    case (guard)
	       'b00: out = din;
	       'b01: out = din;
	       'b10: out = (lsb(din.sfd) == 1) ? din_inc : din;
	       'b11: out = din_inc;
	    endcase
	 end

	 Rnd_Nearest_Away_Zero:
	 begin
	    case (guard)
	       'b00: out = din;
	       'b01: out = din_inc;
	       'b10: out = din_inc;
	       'b11: out = din_inc;
	    endcase
	 end

	 Rnd_Plus_Inf:
	 begin
	    if (guard == 0)
	       out = din;
	    else if (din.sign)
	       out = din;
	    else
	       out = din_inc;
	 end

	 Rnd_Minus_Inf:
	 begin
	    if (guard == 0)
	       out = din;
	    else if (din.sign)
	       out = din_inc;
	    else
	       out = din;
	 end

	 Rnd_Zero:
	 begin
	    out = din;
	 end
      endcase
   end

   if (isInfinity(out)) begin
      exc.overflow = True;
   end

   return tuple2(out,exc);
endfunction

function Tuple3#(FloatingPoint#(e,m),Bit#(2),Exception) normalize( FloatingPoint#(e,m) din, Bit#(x) sfdin )
   provisos(
      Add#(1, a__, x),
      Add#(m, b__, x),
      // per request of bsc
      Add#(c__, TLog#(TAdd#(1, x)), TAdd#(e, 1))
      );

   FloatingPoint#(e,m) out = din;
   Bit#(2) guard = 0;
   Exception exc = defaultValue;

   Int#(TAdd#(e,1)) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out)));
   let zeros = countZerosMSB(sfdin);

   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      guard = '1;
      exc.overflow = True;
      exc.inexact = True;
   end
   else begin
      if (zeros == 0) begin
	 // carry, no sfd adjust necessary

	 if (out.exp == 0)
	    out.exp = 2;
	 else
	    out.exp = out.exp + 1;

	 // carry bit
	 sfdin = sfdin << 1;
      end
      else if (zeros == 1) begin
	 // already normalized

	 if (out.exp == 0)
	    out.exp = 1;

	 // carry, hidden bits
	 sfdin = sfdin << 2;
      end
      else if (zeros == fromInteger(valueOf(x))) begin
	 // exactly zero
	 out.exp = 0;
      end
      else begin
	 // try to normalize
	 Int#(TAdd#(e,1)) shift = zeroExtend(unpack(pack(zeros - 1)));
	 Int#(TAdd#(e,1)) maxshift = exp - fromInteger(minexp(out));

	 if (shift > maxshift) begin
	    // result will be subnormal

	    sfdin = sfdin << maxshift;
	    out.exp = 0;
	 end
	 else begin
	    // result will be normal

	    sfdin = sfdin << shift;
	    out.exp = out.exp - truncate(pack(shift));
	 end

 	 // carry, hidden bits
	 sfdin = sfdin << 2;
      end

      out.sfd = unpack(truncateLSB(sfdin));
      sfdin = sfdin << fromInteger(valueOf(m));

      guard[1] = unpack(truncateLSB(sfdin));
      sfdin = sfdin << 1;

      guard[0] = |sfdin;
   end

   if ((out.exp == 0) && (guard != 0))
      exc.underflow = True;

   return tuple3(out,guard,exc);
endfunction

function Bool isNaNOrInfinity( FloatingPoint#(e,m) din );
   return (din.exp == '1);
endfunction

module mkFpMac(Server#(Tuple4#(Maybe#(FloatingPoint#(e,m)), FloatingPoint#(e,m), FloatingPoint#(e,m), RoundMode), Tuple2#(FloatingPoint#(e,m),Exception)))
   provisos(
      Add#(e,2,ebits),
      Add#(m,1,mbits),
      Add#(m,5,m5bits),
      Add#(mbits,mbits,mmbits),
      // per request of bsc
      Add#(1, a__, mmbits),
      Add#(m, b__, mmbits),
      Add#(c__, TLog#(TAdd#(1, mmbits)), TAdd#(e, 1)),
      Add#(d__, TLog#(TAdd#(1, m5bits)), TAdd#(e, 1)),
      Add#(1, TAdd#(1, TAdd#(m, 3)), m5bits)
      );

   FIFO#(Tuple4#(Maybe#(FloatingPoint#(e,m)),
		 FloatingPoint#(e,m),
		 FloatingPoint#(e,m),
		 RoundMode)) fOperand_S0 <- mkLFIFO;

   FIFO#(Tuple7#(CommonState#(e,m),
		 Bool,
		 FloatingPoint#(e,m),
		 Bool,
		 Int#(ebits),
		 Bit#(mbits),
		 Bit#(mbits))) fState_S1 <- mkLFIFO;

   // check operands, compute exponent for multiply
   rule s1_stage;
      match { .mopA, .opB, .opC, .rmode } <- toGet(fOperand_S0).get;

      CommonState#(e,m) s = CommonState {
	 res: tagged Invalid,
	 exc: defaultValue,
	 rmode: rmode
	 };

      Bool acc = False;
      FloatingPoint#(e,m) opA = 0;

      if (mopA matches tagged Valid .opA_) begin
	 opA = opA_;
	 acc = True;
      end

      Int#(ebits) expB = isSubNormal(opB) ? fromInteger(minexp(opB)) : signExtend(unpack(unbias(opB)));
      Int#(ebits) expC = isSubNormal(opC) ? fromInteger(minexp(opC)) : signExtend(unpack(unbias(opC)));

      Bit#(mbits) sfdB = { getHiddenBit(opB), opB.sfd };
      Bit#(mbits) sfdC = { getHiddenBit(opC), opC.sfd };

      Bool sgnBC = opB.sign != opC.sign;
      Bool infBC = isInfinity(opB) || isInfinity(opC);
      Bool zeroBC = isZero(opB) || isZero(opC);
      Int#(ebits) expBC = expB + expC;

      if (isSNaN(opA)) begin
	 s.res = tagged Valid nanQuiet(opA);
	 s.exc.invalid_op = True;
      end
      else if (isSNaN(opB)) begin
	 s.res = tagged Valid nanQuiet(opB);
	 s.exc.invalid_op = True;
      end
      else if (isSNaN(opC)) begin
	 s.res = tagged Valid nanQuiet(opC);
	 s.exc.invalid_op = True;
      end
      else if (isQNaN(opA)) begin
	 s.res = tagged Valid opA;
      end
      else if (isQNaN(opB)) begin
	 s.res = tagged Valid opB;
      end
      else if (isQNaN(opC)) begin
	 s.res = tagged Valid opC;
      end
      else if ((isInfinity(opB) && isZero(opC)) || (isZero(opB) && isInfinity(opC)) || (isInfinity(opA) && infBC && (opA.sign != sgnBC))) begin
	 // product of zero and infinity or addition of opposite sign infinity
	 s.res = tagged Valid qnan();
	 s.exc.invalid_op = True;
      end
      else if (isInfinity(opA)) begin
	 s.res = tagged Valid opA;
      end
      else if (infBC) begin
	 s.res = tagged Valid infinity(sgnBC);
      end
      else if (isZero(opA) && zeroBC && (opA.sign == sgnBC)) begin
	 s.res = tagged Valid opA;
      end

      fState_S1.enq(tuple7(s,
			   acc,
			   opA,
			   sgnBC,
			   expBC,
			   sfdB,
			   sfdC));
   endrule

   FIFO#(Tuple5#(CommonState#(e,m),
		 Bool,
		 FloatingPoint#(e,m),
		 Bool,
		 Int#(ebits))) fState_S2 <- mkLFIFO;
   FIFO#(Bit#(mmbits)) fProd_S2 <- mkLFIFO;

   // start multiply
   rule s2_stage;
      match { .s, .acc, .opA, .sgnBC, .expBC, .sfdB, .sfdC } <- toGet(fState_S1).get;

      let sfdBC = primMul(sfdB, sfdC);

      fProd_S2.enq(sfdBC);

      fState_S2.enq(tuple5(s,
			   acc,
			   opA,
			   sgnBC,
			   expBC));
   endrule

   FIFO#(Tuple5#(CommonState#(e,m),
		 Bool,
		 FloatingPoint#(e,m),
		 Bool,
		 Int#(ebits))) fState_S3 <- mkLFIFO;
   FIFO#(Bit#(mmbits)) fProd_S3 <- mkLFIFO;

   // passthrough stage for multiply register retiming
   rule s3_stage;
      match { .s, .acc, .opA, .sgnBC, .expBC } <- toGet(fState_S2).get;
      let sfdBC <- toGet(fProd_S2).get;

      fProd_S3.enq(sfdBC);

      fState_S3.enq(tuple5(s,
			   acc,
			   opA,
			   sgnBC,
			   expBC));
   endrule

   FIFO#(Tuple5#(CommonState#(e,m),
		 Bool,
		 FloatingPoint#(e,m),
		 FloatingPoint#(e,m),
		 Bit#(2))) fState_S4 <- mkLFIFO;

   // normalize multiplication result
   rule s4_stage;
      match { .s, .acc, .opA, .sgnBC, .expBC } <- toGet(fState_S3).get;
      let sfdBC <- toGet(fProd_S3).get;

      FloatingPoint#(e,m) bc = defaultValue;
      Bit#(2) guardBC = ?;

      if (s.res matches tagged Invalid) begin
	 if (expBC > fromInteger(maxexp(bc))) begin
	    bc.sign = sgnBC;
	    bc.exp = maxBound - 1;
	    bc.sfd = maxBound;
	    guardBC = '1;

	    s.exc.overflow = True;
	    s.exc.inexact = True;
	 end
	 else if (expBC < (fromInteger(minexp_subnormal(bc))-2)) begin
	    bc.sign = sgnBC;
	    bc.exp = 0;
	    bc.sfd = 0;
	    guardBC = 0;

	    if (|sfdBC == 1) begin
	       guardBC = 1;
	       s.exc.underflow = True;
	       s.exc.inexact = True;
	    end
	 end
	 else begin
	    let shift = fromInteger(minexp(bc)) - expBC;
	    if (shift > 0) begin
	       // subnormal
	       Bit#(1) sfdlsb = |(sfdBC << (fromInteger(valueOf(mmbits)) - shift));

	       sfdBC = sfdBC >> shift;
	       sfdBC[0] = sfdBC[0] | sfdlsb;

	       bc.exp = 0;
	    end
	    else begin
	       bc.exp = cExtend(expBC + fromInteger(bias(bc)));
	    end

	    bc.sign = sgnBC;
	    let y = normalize(bc, sfdBC);
	    bc = tpl_1(y);
	    guardBC = tpl_2(y);
	    s.exc = s.exc | tpl_3(y);
	 end
      end

      fState_S4.enq(tuple5(s,
			   acc,
			   opA,
			   bc,
			   guardBC));
   endrule

   FIFO#(Tuple8#(CommonState#(e,m),
		 Bool,
		 Bool,
		 Bool,
		 Int#(ebits),
		 Int#(ebits),
		 Bit#(m5bits),
		 Bit#(m5bits))) fState_S5 <- mkLFIFO;

   // calculate shift and sign for add
   rule s5_stage;
      match { .s, .acc, .opA, .opBC, .guardBC } <- toGet(fState_S4).get;

      Int#(ebits) expA = isSubNormal(opA) ? fromInteger(minexp(opA)) : signExtend(unpack(unbias(opA)));
      Int#(ebits) expBC = isSubNormal(opBC) ? fromInteger(minexp(opBC)) : signExtend(unpack(unbias(opBC)));

      Bit#(m5bits) sfdA = {1'b0, getHiddenBit(opA), opA.sfd, 3'b0};
      Bit#(m5bits) sfdBC = {1'b0, getHiddenBit(opBC), opBC.sfd, guardBC, 1'b0};

      Bool sub = opA.sign != opBC.sign;

      Int#(ebits) exp = ?;
      Int#(ebits) shift = ?;
      Bit#(m5bits) x = ?;
      Bit#(m5bits) y = ?;
      Bool sgn = ?;

      if ((!acc) || (expBC > expA) || ((expBC == expA) && (sfdBC > sfdA))) begin
	 exp = expBC;
	 shift = expBC - expA;
	 x = sfdBC;
	 y = sfdA;
	 sgn = opBC.sign;
      end
      else begin
	 exp = expA;
	 shift = expA - expBC;
	 x = sfdA;
	 y = sfdBC;
	 sgn = opA.sign;
      end

      fState_S5.enq(tuple8(s,
			   acc,
			   sub,
			   sgn,
			   exp,
			   shift,
			   x,
			   y));
   endrule

   FIFO#(Tuple7#(CommonState#(e,m),
		 Bool,
		 Bool,
		 Bool,
		 Int#(ebits),
		 Bit#(m5bits),
		 Bit#(m5bits))) fState_S6 <- mkLFIFO;

   // shift second add operand
   rule s6_stage;
      match { .s, .acc, .sub, .sgn, .exp, .shift, .x, .y } <- toGet(fState_S5).get;

      if (s.res matches tagged Invalid) begin
	 if (shift < fromInteger(valueOf(m5bits))) begin
	    Bit#(m5bits) guard;

	    guard = y << (fromInteger(valueOf(m5bits)) - shift);
	    y = y >> shift;
	    y[0] = y[0] | (|guard);
	 end
	 else if (|y == 1) begin
	    y = 1;
	 end
      end

      fState_S6.enq(tuple7(s,
			   acc,
			   sub,
			   sgn,
			   exp,
			   x,
			   y));
   endrule

   FIFO#(Tuple7#(CommonState#(e,m),
		 Bool,
		 Bool,
		 Bool,
		 Int#(ebits),
		 Bit#(m5bits),
		 Bit#(m5bits))) fState_S7 <- mkLFIFO;

   // add/subtract sfd
   rule s7_stage;
      match { .s, .acc, .sub, .sgn, .exp, .x, .y } <- toGet(fState_S6).get;

      let sum = x + y;
      let diff = x - y;

      fState_S7.enq(tuple7(s,
			   acc,
			   sub,
			   sgn,
			   exp,
			   sum,
			   diff));
   endrule

   FIFO#(Tuple5#(CommonState#(e,m),
		 Bool,
		 FloatingPoint#(e,m),
		 Bit#(2),
		 Bool)) fState_S8 <- mkLFIFO;

   // normalize addition result
   rule s8_stage;
      match { .s, .acc, .sub, .sgn, .exp, .sum, .diff } <- toGet(fState_S7).get;

      FloatingPoint#(e,m) out = defaultValue;
      Bit#(2) guard = 0;

      if (s.res matches tagged Invalid) begin
	 Bit#(m5bits) sfd;

	 sfd = sub ? diff : sum;

	 out.sign = sgn;
	 out.exp = cExtend(exp + fromInteger(bias(out)));

	 let y = normalize(out, sfd);
	 out = tpl_1(y);
	 guard = tpl_2(y);
	 s.exc = s.exc | tpl_3(y);
      end

      fState_S8.enq(tuple5(s,
			   acc,
			   out,
			   guard,
			   sub));
   endrule

   FIFO#(Tuple2#(FloatingPoint#(e,m),Exception)) fResult_S9 <- mkLFIFO;

   // round result
   rule s9_stage;
      match { .s, .acc, .out, .guard, .sub } <- toGet(fState_S8).get;

      if (s.res matches tagged Valid .x) begin
	 out = x;
      end
      else begin
	 let y = round(s.rmode, out, guard);
	 out = tpl_1(y);
	 s.exc = s.exc | tpl_2(y);

	 // adjust sign for exact zero result
	 if (acc && isZero(out) && !s.exc.inexact && sub) begin
	    out.sign = (s.rmode == Rnd_Minus_Inf);
	 end
      end

      fResult_S9.enq(tuple2(out,s.exc));
   endrule

   interface request = toPut(fOperand_S0);
   interface response = toGet(fResult_S9);

endmodule

////////////////////////////////////////////////////////////////////////////////
