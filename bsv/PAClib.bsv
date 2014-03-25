// ****************************************************************
// Copyright (c) 2009-2013, Bluespec, Inc.  All Rights Reserved.
//
// This Bluespec SystemVerilog Library is provided by Bluespec, Inc. to
// its customers under license agreements.
//
// Version 1.3, April 11, 2013, 13:00 EDT
//  Edited version by Nikhil -- April 2013
//
//
// ****************************************************************
// SVN Version tags
// $Revision: 31013 $
// $Date: 2013-04-11 15:21:01 -0400 (Thu, 11 Apr 2013) $
// ****************************************************************

package PAClib;

import FIFO              :: *;
import FIFOF             :: *;
import SpecialFIFOs      :: *;
import BRAMFIFO          :: *;
import GetPut            :: *;
import ClientServer      :: *;
import Connectable       :: *;
import Vector            :: *;
import CompletionBuffer  :: *;

// ****************************************************************
// Define FPGA_TARGET or not, as desired.
// In the FPGA case, SizedFIFOFs are implemented in BRAMs

// `define FPGA_TARGET

`ifdef FPGA_TARGET

module mkSizedFIFOF_local #(Integer n) (FIFOF #(t))
   provisos (Bits #(t, tsiz),
	     Add  #(1, _any, tsiz));
   let ifc <- mkSizedBRAMFIFOF (n);    // **** when targeting an FPGA
   return ifc;
endmodule

`else

module mkSizedFIFOF_local #(Integer n) (FIFOF #(t))
   provisos (Bits #(t, tsiz));
   let ifc <- mkSizedFIFOF (n);        // **** otherwise
   return ifc;
endmodule

`endif

// ****************************************************************
// **************** TYPES FOR PIPE OUTPUTS AND PIPES
// ****************************************************************

// ----------------------------------------------------------------
// The output of a Pipe component is the output subset of the FIFOF interface,
// with the same semantics

interface PipeOut #(type t);
   method t       first ();
   method Action  deq ();
   method Bool    notEmpty ();
endinterface

// ----------------------------------------------------------------
// A Pipe component is module with a PipeOut parameter and a Pipeout interface
// i.e., a function from Pipeout to Module #(PipeOut)

typedef (function Module #(PipeOut #(tb)) mkFoo (PipeOut #(ta) ifc))
        Pipe#(type ta, type tb);

// ----------------------------------------------------------------
// Some useful utility interface-conversion functions

function PipeOut #(a)  f_FIFOF_to_PipeOut  (FIFOF #(a) fifof);
   return (interface PipeOut;
              method a first ();
                 return fifof.first;
              endmethod
              method Action deq ();
                 fifof.deq;
              endmethod
              method Bool notEmpty ();
                 return fifof.notEmpty ();
              endmethod
           endinterface);
endfunction

module mkFIFOF_to_Pipe
                #(FIFOF #(a)    fifof,
                  PipeOut #(a)  po_in)
                (PipeOut #(a));

   rule rl_input_to_fifo;
      fifof.enq (po_in.first ());  po_in.deq ();
   endrule

   return f_FIFOF_to_PipeOut (fifof);
endmodule: mkFIFOF_to_Pipe

instance ToGet #(PipeOut #(a), a);
   function Get #(a) toGet (PipeOut #(a) po);
      return (interface Get;
                 method ActionValue #(a) get ();
                    po.deq ();
                    return po.first ();
                 endmethod
              endinterface);
   endfunction
endinstance

// ----------------------------------------------------------------
// Convert a Pipe into a server
// Adds a fifo buffer in front of the pipe

module [Module] mkPipe_to_Server
                #(Pipe #(ta, tb) pipe)
                (Server #(ta, tb))
        provisos (Bits #(ta, wd_ta));

   FIFOF #(ta)   fifof  <- mkLFIFOF;
   PipeOut #(tb) serverPipe <- pipe (f_FIFOF_to_PipeOut (fifof));

   return (interface Server;
              interface Put request  = toPut (fifof);
              interface Get response = toGet (serverPipe);
           endinterface);
endmodule: mkPipe_to_Server

// ****************************************************************
// **************** SOURCES AND SINKS
// ****************************************************************

// ----------------------------------------------------------------
// Source values from an ActionValue function
// Contains a buffer to hold the value from the function

module mkSource_from_fav
                #(ActionValue #(a) f_avf)
                (PipeOut #(a))
 provisos (Bits #(a, sa));

   FIFOF #(a) fifof <- mkLFIFOF;

   rule rl_source;
      let x <- f_avf ();
      fifof.enq (x);
   endrule

   return f_FIFOF_to_PipeOut (fifof);
endmodule: mkSource_from_fav

// ----------------------------------------------------------------
// Source a constant x

module mkSource_from_constant
                #(a x)
                (PipeOut #(a));
   method a first ();
      return x;
   endmethod

   method Action deq ();
      noAction;
   endmethod

   method Bool notEmpty ();
      return True;
   endmethod
endmodule: mkSource_from_constant

// ----------------------------------------------------------------
// Sinks values and discards them

module mkSink
                #(PipeOut #(a) po_in)
                (Empty);

   rule rl_drain;
      po_in.deq;
   endrule
endmodule: mkSink

// ----------------------------------------------------------------
// Sinks values into an Action function

module mkSink_to_fa
                #(function Action f_a (a x),
                  PipeOut #(a) po_in)
                (Empty);

   rule rl_drain;
      f_a (po_in.first);
      po_in.deq;
   endrule
endmodule: mkSink_to_fa

// ****************************************************************
// **************** SIMPLE PIPELINE BUFFERS
// ****************************************************************
// Simple buffers

// ----------------------------------------------------------------
// 1-stage delay buffer (asynchronous/ flow-controlled/ FIFO-like)

module mkBuffer
                #(PipeOut #(a) po_in)
                (PipeOut #(a))
 provisos (Bits #(a,sa));

   FIFOF #(a) fifof <- mkLFIFOF;

   rule rl_into_buffer;
      fifof.enq (po_in.first);
      po_in.deq;
   endrule

   return f_FIFOF_to_PipeOut (fifof);
endmodule: mkBuffer

// ----------------------------------------------------------------
// n-stage delay buffer (asynchronous/ flow-controlled/ FIFO-like)

module mkBuffer_n
                #(Integer n,
                  PipeOut #(a) po_in)
                (PipeOut #(a))
`ifdef FPGA_TARGET
   provisos (Bits #(a,sa), Add  #(1,_any,sa));
`else
   provisos (Bits #(a,sa));
`endif

   FIFOF #(a) fifof <- mkSizedFIFOF_local (n);

   rule rl_into_buffer;
      fifof.enq (po_in.first);
      po_in.deq;
   endrule

   return f_FIFOF_to_PipeOut (fifof);
endmodule: mkBuffer_n

// ----------------------------------------------------------------
// 1-stage synchronous delay buffer (register-like)
// init_value is the value initial value in the buffer register

module mkSynchBuffer
                #(a             init_value,
                  PipeOut #(a)  po_in)
                (PipeOut #(a))
 provisos (Bits #(a,sa));

   Reg #(a) rg <- mkReg (init_value);

   method a first ();
      return rg;
   endmethod

   method Action deq ();
      rg <= po_in.first ();
      po_in.deq ();
   endmethod

   method Bool notEmpty ();
      return po_in.notEmpty ();
   endmethod
endmodule: mkSynchBuffer

// ----------------------------------------------------------------
// n-stage synchronous delay buffer (register-like)
// init_value is the value initial value in the buffer registers

module mkSynchBuffer_n
                #(Integer n,
                  a             init_value,
                  PipeOut #(a)  po_in)
                (PipeOut #(a))
 provisos (Bits #(a,sa));

   PipeOut #(a) syncBuffer_n = po_in;

   for (Integer j = 0; j < n; j = j + 1)
      syncBuffer_n <- mkSynchBuffer (init_value, syncBuffer_n);

   return syncBuffer_n;
endmodule: mkSynchBuffer_n

// ****************************************************************
// **************** WRAP COMBINATIONAL FUNCTION INTO PIPE
// ****************************************************************

// ----------------------------------------------------------------
// Wrap combinational function fn into a pipe

module mkFn_to_Pipe
                #(function tb fn (ta x),
                  PipeOut #(ta) po_in)
                (PipeOut #(tb));

   method tb first ();
      return fn (po_in.first);
   endmethod

   method Action deq ();
      po_in.deq;
   endmethod

   method Bool notEmpty ();
      return po_in.notEmpty;
   endmethod
endmodule: mkFn_to_Pipe

// ----------------------------------------------------------------
// "Tee" the pipe data to an Action function afn
// i.e., apply afn to each value as it passes through.
// (The name comes from the well-known Unix shell "tee" command.)
// No additional buffering.

function PipeOut #(ta) fn_tee_to_Action (function Action  afn (ta x),
					 PipeOut #(ta) po_in)
   = interface PipeOut;
	method ta first = po_in.first;

	method Action deq;
	   afn (po_in.first);
	   po_in.deq;
	endmethod

	method Bool notEmpty = po_in.notEmpty;
     endinterface;

// ----------------------------------------------------------------
// Wrap ActionValue function fn into a pipe; always has buffer after fn

module mkAVFn_to_Pipe
                #(function ActionValue #(tb) avfn (ta x),
                  PipeOut #(ta) po_in)
                (PipeOut #(tb))
 provisos (Bits #(ta, wd_ta),
	   Bits #(tb, wd_tb));
		       
   FIFOF #(tb) fifof <- mkPipelineFIFOF;
   
   rule rl_do_avfn;
      tb y <- avfn (po_in.first);
      fifof.enq (y);
      po_in.deq;
   endrule

   return f_FIFOF_to_PipeOut (fifof);
endmodule: mkAVFn_to_Pipe

// ----------------------------------------------------------------
// Wrap combinational function fn into a pipe, with optional buffering

module mkFn_to_Pipe_Buffered
                #(Bool param_buf_before,
                  function b fn (a x),
                  Bool param_buf_after,
                  PipeOut #(a) po_in)
                (PipeOut #(b))
 provisos (Bits #(a, sa),
           Bits #(b, sb));

   PipeOut #(a) preFIFO = po_in;

   if (param_buf_before)
      preFIFO <- mkBuffer (po_in);

   PipeOut #(b) postFIFO <- mkFn_to_Pipe (fn, preFIFO);

   if (param_buf_after)
      postFIFO <- mkBuffer (postFIFO);

   return postFIFO;
endmodule: mkFn_to_Pipe_Buffered

// ----------------------------------------------------------------
// Wrap combinational function fn into a pipe, with optional synchronous buffering

module mkFn_to_Pipe_SynchBuffered
                #(Maybe #(a) param_buf_before,
                  function b fn (a x),
                  Maybe #(b) param_buf_after,
                  PipeOut #(a) po_in)
                (PipeOut #(b))
 provisos (Bits #(a, sa),
           Bits #(b, sb));

   PipeOut #(a) preBuffer = po_in;

   case (param_buf_before) matches
      tagged Valid .init_value_a : preBuffer <- mkSynchBuffer (init_value_a, po_in);
      tagged Invalid             : begin end
   endcase

   PipeOut #(b) postBuffer <- mkFn_to_Pipe (fn, preBuffer);

   case (param_buf_after) matches
      tagged Valid .init_value_b : postBuffer <- mkSynchBuffer (init_value_b, postBuffer);
      tagged Invalid             : begin end
   endcase

   return postBuffer;
endmodule: mkFn_to_Pipe_SynchBuffered

// ----------------------------------------------------------------
// Wrap a function in a Pipe and add n stages of registers to allow for retiming.

module [Module] mkRetimedPipelineFn (function b func (a x)
                                     ,Integer stages
                                     ,PipeOut#(a) pin
                                     ,PipeOut#(b) ifc
                            )
   provisos (//Bits#(a,sa)
             Bits#(b,sb)
             );

   PipeOut#(b) retimedPipe[stages+1] ;
   retimedPipe[0] <- mkFn_to_Pipe(func, pin);

   for (Integer i = 1; i < stages; i = i + 1) begin
      retimedPipe[i] <- mkBuffer(retimedPipe[i-1]);
   end

   PipeOut#(b) retimedFinal <- mkBuffer_n(2,retimedPipe[stages-1]);
   return retimedFinal;
endmodule

// ----------------------------------------------------------------
// "Tap" a pipe by applying an Action tap_fn_a () to each value flowing through

module mkTap
                #(function Action tap_fn_a (a x),
                  PipeOut #(a) po_in)
                (PipeOut #(a));

   method a first ();
      return po_in.first;
   endmethod

   method Action deq ();
      tap_fn_a (po_in.first);
      po_in.deq;
   endmethod

   method Bool notEmpty ();
      return po_in.notEmpty ();
   endmethod
endmodule

// ****************************************************************
// **************** FUNNELING AND UNFUNNELING
// Terminology:
//  Funneling   "serializes"   an mk-vector down to a k-stream of m-vector slices
//  Unfunneling "unserializes" a  k-stream of m-vector slices up to an mk-vector
// ****************************************************************

// ----------------------------------------------------------------
// Funnel an mk-vector down to a k-stream of m-vector slices

module mkFunnel
                #(PipeOut #(Vector #(mk, a)) po_in)
                (PipeOut #(Vector #(m, a)))

        provisos (Bits #(a, sa),
                  Add #(ignore_1, 1, mk),    // mk > 0               (assert)
                  Add #(ignore_2, 1, m),     // m > 0                (assert)
                  Add #(ignore_3, m, mk),    // m <= mk              (assert)
                  Mul #(m, k, mk),           // m * k == mk          (derive k)
                  Log #(k, logk),            // log (k) == logk      (derive logk)

                  // The following two provisos are redundant, but currently required by bsc
                  // TODO: recheck periodically if bsc improvements make these redundant
                  Mul #(mk, sa, TMul #(k, TMul #(m, sa)))
                 );

   UInt #(logk)                k_minus_1  =  fromInteger (valueof(k) - 1);
   Reg #(UInt #(logk))         index_k    <- mkReg (0);
   Vector #(k, Vector #(m, a)) values     = unpack (pack (po_in.first));

   return (interface PipeOut;
               method Vector #(m,a) first ();
                  return values [index_k];
               endmethod

               method Action deq ();
                  if (index_k == k_minus_1) begin
                     index_k <= 0;
                     po_in.deq ();
                  end
                  else
                     index_k <= index_k + 1;
               endmethod

               method Bool notEmpty ();
                  return  po_in.notEmpty ();
               endmethod
           endinterface);
endmodule: mkFunnel

// ----------------------------------------------------------------
// Funnel an mk-vector down to a k-stream of m-vector slices accompanied by their indexes

module mkFunnel_Indexed
                #(PipeOut #(Vector #(mk, a)) po_in)
                (PipeOut #(Vector #(m, Tuple2 #(a, UInt #(logmk)))))

        provisos (Bits #(a, sa),
                  Add #(ignore_1, 1, mk),    // mk > 0               (assert)
                  Add #(ignore_2, 1, m),     // m > 0                (assert)
                  Add #(ignore_3, m, mk),    // m <= mk              (assert)
                  Log #(mk, logmk),          // log (mk) == logmk    (derive logmk)
                  Mul #(m, k, mk),           // m * k == mk          (derive k)
                  Log #(k, logk),            // log (k) == logk      (derive logk)

                  // The following two provisos are redundant, but currently required by bsc
                  Bits #(Vector #(k, Vector #(m, a)), TMul #(mk, sa)),
                  Bits #(Vector #(mk, a), TMul #(mk, sa)));

   UInt #(logk) k_minus_1 = fromInteger (valueof(k) - 1);

   // This function pairs each input vector element with an index, starting from base
   function Vector #(n, Tuple2 #(a, UInt #(logmk)))
            attach_indexes_from_base (UInt #(logmk) base, Vector #(n, a)  xs);

      function UInt #(logmk) add_base (Integer j) = (base + fromInteger (j));

      let indexes     = genWith (add_base);   // {base,base+1,...,base+n-1}
      let x_index_vec = zip (xs, indexes);    // {(x0,base),(x1,base+11),...,(xmk-1,base+n-1)}
      return x_index_vec;
   endfunction: attach_indexes_from_base

   Reg #(UInt #(logk))                index_k    <- mkReg (0);
   Reg #(UInt #(logmk))               index_mk   <- mkReg (0);

   return (interface PipeOut;
               method Vector #(m, Tuple2 #(a, UInt #(logmk))) first ();
                  Vector #(k, Vector #(m, a)) k_vec_m_vec = unpack (pack (po_in.first ()));
                  return attach_indexes_from_base (index_mk, k_vec_m_vec [index_k]);
               endmethod

               method Action deq ();
                  if (index_k == k_minus_1) begin
                     po_in.deq ();
                     index_k <= 0;
                     index_mk <= 0;
                  end
                  else begin
                     index_k <= index_k + 1;
                     index_mk <= index_mk + fromInteger (valueof (m));
                  end
               endmethod

               method Bool notEmpty ();
                  return po_in.notEmpty ();
               endmethod
           endinterface);
endmodule: mkFunnel_Indexed

// ----------------------------------------------------------------
// Unfunnel a k-stream of m-vector slices up to an mk-vector
// When k > 1, of course one needs buffering
// When k==1, it is conceptually the identity function and buffering is optional
//            and the parameter 'state_if_k_is_1' controls whether to insert a buffer or not

module mkUnfunnel
                #(Bool                      state_if_k_is_1,
                  PipeOut #(Vector #(m,a))  po_in)
                (PipeOut #(Vector #(mk, a)))

        provisos (Bits #(a, sa),
                  Add #(ignore_1, 1, mk),    // assert mk > 0
                  Add #(ignore_2, 1, m),     // assert m > 0
                  Add #(ignore_3, m, mk),    // m <= mk
                  Log #(mk, logmk),          // derive logmk
                  Mul #(m, k, mk));          // derive k

   if ((valueof (k) == 1) && (! state_if_k_is_1)) begin
      // ---- Here, m == mk, and don't allocate any registers (bypass straight through)

      function Vector #(mk, a)  f_pseudo_extend (Vector #(m, a)  xs);
          // The following is to appease the typechecker because in general m != mk
          // The 'newVector' below will be empty
          return append (xs, newVector);
      endfunction

      let ifc <- mkFn_to_Pipe (f_pseudo_extend, po_in);
      return ifc;
   end

   else begin
      // ---- Here, m < mk (or m==mk and  'state_if_k_is_1'  is True)

      Vector #(k, Reg #(Vector #(m, a)))  values     <- replicateM (mkRegU);
      Reg #(UInt #(logmk))                index      <- mkReg (0);
      Reg #(Bool)                         full       <- mkReg (False);
      Wire #(Bool)                        is_getting <- mkDWire (False);
      Wire #(Bool)                        is_putting <- mkDWire (False);

      UInt #(logmk) k_minus_1 = fromInteger (valueof(k) - 1);

      function Action fa_update_state (UInt #(logmk) put_ix);
         action
            index <= ((put_ix == k_minus_1 ? 0 : put_ix+1));
            full  <= (put_ix == k_minus_1);
         endaction
      endfunction

      (* fire_when_enabled, no_implicit_conditions *)
      rule rUpdate_state;
         if (is_getting && is_putting)
            fa_update_state (0);

         else if (is_putting)
            fa_update_state (index);

         else if (is_getting)
            full <= False;
      endrule

      rule rl_receive ((! full) || is_getting);
         values [index] <= po_in.first ();
         po_in.deq ();
         is_putting <= True;
      endrule

      return (interface PipeOut;
                 method Vector #(mk, a) first () if (full);
                    Vector #(k, Vector #(m,a)) ys = readVReg (values);
                    Vector #(mk, a) result = concat (ys);
                    return  result;
                 endmethod

                 method Action deq () if (full);
                    is_getting <= True;
                 endmethod

                 method Bool notEmpty ();
                    return full;
                 endmethod
              endinterface);
   end
endmodule: mkUnfunnel

// ****************************************************************
// **************** MAPS (PARALLEL POINTWISE APPLICATIONS)
// ****************************************************************

// ----------------------------------------------------------------
// Note: PAClib does not contain a mkMap_fn module because it's easily
// defined with mkFn_to_Pipe:
//
//    mkMap_fn (f) == mkFn_to_Pipe (map (f));
//
// It is functionally equivalent to:   mkMap_pipe (mkFn_to_Pipe (f))
// but a little cheaper since mkMap_pipe also instantiates n BypassFIFOs (see below)

// ----------------------------------------------------------------
// Make a pipe that maps pipe mkP over an n-vector
// I.e., each datum from an input n-vector is fed through a copy of mkP
// and the results are collected in an output n-vector
// The latencies of the different mkP pipes can vary, and can be dynamically data-dependent,
// but if each mkP (is assumed to) maintain FIFO order, the vectors will be in sync

module [Module] mkMap
                #(Pipe #(a,b) mkP,
                  PipeOut #(Vector #(n, a)) po_in)

                (PipeOut #(Vector #(n, b)))

        provisos (Bits #(a, sa));

   Vector #(n, FIFO #(Bit #(0))) mkMapTakenMarkers <- replicateM (mkBypassFIFO);

   rule rl_deq;                 // Dequeue the pipe_in when all map-pipes have fired.
      po_in.deq ();
      for (Integer j = 0; j < valueof (n); j = j + 1)
         mkMapTakenMarkers [j].deq;
   endrule

   function  PipeOut #(a) genIfc (Integer j);
      return (interface PipeOut;
                             method a first ();
                                return po_in.first [j];
                             endmethod

                             method Action deq ();
                                mkMapTakenMarkers [j].enq (?);
                             endmethod

                             method Bool notEmpty ();
                                return ?;    // don't care
                             endmethod
           endinterface);
   endfunction

   Vector #(n, PipeOut #(b)) mkMapPipeElem <- mapM (mkP, map(genIfc, genVector));

   method Vector #(n, b) first ();
      function b get_first (PipeOut #(b) po) = po.first ();
      return map (get_first, mkMapPipeElem);
   endmethod

   method Action deq ();
      for (Integer j = 0; j < valueof (n); j = j + 1)
         mkMapPipeElem[j].deq ();
   endmethod

   method Bool notEmpty ();
      return po_in.notEmpty ();
   endmethod
endmodule: mkMap

// ----------------------------------------------------------------
// Make pipe that maps pipe mkP over an mk-vector
// I.e., each datum from the input mk-vector is fed through a copy of mkP
// and the results are collected in the output mk-vector
// The input mk-vector is funneled into a k-stream of m-vectors accompanied by their indexes
//        param_buf_funnel specifies if a buffer is desired when k = 1
// Applies m copies of mkP in parallel
// Unfunnels the k-stream of m-vector results into an mk-vector
//        param_buf_unfunnel specifies if a buffer is desired when k = 1

module [Module] mkMap_with_funnel_indexed
                #(UInt #(m) dummy_m,    // Only used to pass 'm'; dummy_m is ignored
                  Pipe #(Tuple2 #(a, UInt #(logmk)), b) mkP,
                  Bool param_buf_unfunnel,
                  PipeOut #(Vector #(mk, a)) po_in)
                (PipeOut #(Vector #(mk, b)))

        provisos (Bits #(a, sa),
                  Bits #(b, sb),
                  Mul #(m,k,mk),                        // derive k
                  Log #(mk, logmk),                     // derive logmk

                  Bits #(Vector #(k, Vector #(m, a)), TMul #(mk, sa)),
                  Bits #(Vector #(mk, a), TMul #(mk, sa)),

                  Add #(_ignore1, 1, m),                // assert 1 <= m
                  Add #(_ignore2, 1, mk),               // assert 1 <= mk
                  Add #(_ignore3, m, mk));              // assert m <= mk

   if (valueof (m) * valueof (k) != valueof (mk))
      errorM ("mkMap_with_funnel_indexed: m ("   + integerToString (valueof (m))  + ")"
                                    + " * k ("   + integerToString (valueof (k))  + ")"
                                    + " != mk (" + integerToString (valueof (mk)) + ")");

   // ---- Funnel mk-vec into k-stream of m-vecs with indexes (k x m = mk)
   PipeOut #(Vector #(m, Tuple2 #(a, UInt #(logmk))))
      funnel <- mkFunnel_Indexed (po_in);

   // ---- Do the mapping
   PipeOut #(Vector #(m, b))
      mapping <- mkMap (mkP, funnel);

   // ---- Unfunnel k-stream of m-vecs into mk-vec
   PipeOut #(Vector #(mk, b))
      unfunnel <- mkUnfunnel (param_buf_unfunnel, mapping);

   return  unfunnel;
endmodule: mkMap_with_funnel_indexed

// ----------------------------------------------------------------
// The following is functionally equivalent to useing mkMap_with_funnel_indexed,
// but a little cheaper because for the embedded mapping, it just uses map(fn)
// instead of mkMap, which is slightly cheaper (see comment at mkMap)

// Make pipe that maps function fn() over an mk-vector
// I.e., each datum from the input mk-vector is fed through a copy of fn()
// and the results are collected in the output mk-vector
// The input mk-vector is funneled into a k-stream of m-vectors accompanied by their indexes
//        param_buf_funnel specifies if a buffer is desired when k = 1
// Applies m copies of mkP in parallel
// Unfunnels the k-stream of m-vector results into an mk-vector
//        param_buf_unfunnel specifies if a buffer is desired when k = 1

module [Module] mkMap_fn_with_funnel_indexed
                #(UInt #(m) dummy_m,    // Only used to pass 'm'; dummy_m is ignored
                  function b fn (Tuple2 #(a, UInt #(logmk)) xj),
                  Bool param_buf_unfunnel,
                  PipeOut #(Vector #(mk, a)) po_in)
                (PipeOut #(Vector #(mk, b)))

        provisos (Bits #(a, sa),
                  Bits #(b, sb),
                  Mul #(m,k,mk),                        // derive k
                  Log #(mk, logmk),                     // derive logmk

                  Bits #(Vector #(k, Vector #(m, a)), TMul #(mk, sa)),
                  Bits #(Vector #(mk, a), TMul #(mk, sa)),

                  Add #(_ignore1, 1, m),                // assert 1 <= m
                  Add #(_ignore2, 1, mk),               // assert 1 <= mk
                  Add #(_ignore3, m, mk));              // assert m <= mk

   if (valueof (m) * valueof (k) != valueof (mk))
      errorM ("mkMap_with_funnel_indexed: m ("   + integerToString (valueof (m))  + ")"
                                    + " * k ("   + integerToString (valueof (k))  + ")"
                                    + " != mk (" + integerToString (valueof (mk)) + ")");

   // ---- Funnel mk-vec into k-stream of m-vecs with indexes (k x m = mk)
   PipeOut #(Vector #(m, Tuple2 #(a, UInt #(logmk))))
      funnel <- mkFunnel_Indexed (po_in);

   // ---- Do the mapping
   PipeOut #(Vector #(m, b))
      mapping <- mkFn_to_Pipe (map (fn), funnel);

   // ---- Unfunnel k-stream of m-vecs into mk-vec
   PipeOut #(Vector #(mk, b))
      unfunnel <- mkUnfunnel (param_buf_unfunnel, mapping);

   return  unfunnel;
endmodule: mkMap_fn_with_funnel_indexed

// ****************************************************************
// **************** FORKS AND JOINS
// ****************************************************************

// ----------------------------------------------------------------
// mkFork
// Fork an input stream into two output streams
//     fork_fn is used to split each input item va into two output items (vb,vc)

module mkFork
                #(function Tuple2 #(b, c) fork_fn (a va),
                  PipeOut #(a) poa)
                (Tuple2 #(PipeOut #(b), PipeOut #(c)));

   FIFOF #(Bit #(0)) vb_taken_signal <- mkBypassFIFOF;
   FIFOF #(Bit #(0)) vc_taken_signal <- mkBypassFIFOF;

   rule rl_deq;
      poa.deq ();
      vb_taken_signal.deq ();
      vc_taken_signal.deq ();
   endrule

   match { .vb, .vc } = fork_fn (poa.first ());

   PipeOut #(b) pob = interface PipeOut;
                         method b first() if (vb_taken_signal.notFull);
                            return vb;
                         endmethod
                         method Action deq;
                            vb_taken_signal.enq (?);
                         endmethod
                         method Bool notEmpty;
                            return poa.notEmpty && vb_taken_signal.notFull;
                         endmethod
                      endinterface;

   PipeOut #(c) poc = interface PipeOut;
                         method c first() if (vc_taken_signal.notFull);
                            return vc;
                         endmethod
                         method Action deq;
                            vc_taken_signal.enq (?);
                         endmethod
                         method Bool notEmpty;
                            return poa.notEmpty && vc_taken_signal.notFull;
                         endmethod
                      endinterface;

   return tuple2 (pob, poc);
endmodule: mkFork

// ----------------------------------------------------------------
// mkFork2
// Fork an input stream into two output streams
//     fork_fn is used to split each input item va into two output items (vb,vc)

module mkFork2
                #(function Tuple2 #(b, c) fork_fn (a va),
                  PipeOut #(a) poa)
                (Tuple2 #(PipeOut #(b), PipeOut #(c)))
   provisos (Bits #(a, sa),
	     Bits #(b, sb),
	     Bits #(c, sc));

   FIFOF #(b) fifo_b <- mkPipelineFIFOF;
   FIFOF #(c) fifo_c <- mkPipelineFIFOF;

   rule rl_deq;
      match { .vb, .vc } = fork_fn (poa.first); poa.deq;
      fifo_b.enq (vb);
      fifo_c.enq (vc);
   endrule

   PipeOut #(b) pob = f_FIFOF_to_PipeOut (fifo_b);
   PipeOut #(c) poc = f_FIFOF_to_PipeOut (fifo_c);
   return tuple2 (pob, poc);
endmodule: mkFork2

// ----------------------------------------------------------------
// mkForkVector
// Fork an input stream into a vector of output streams (replicating input data)

module mkForkVector
                #(PipeOut #(a) po)
                (Vector #(n, PipeOut #(a)));

   Vector #(n, FIFOF #(Bit #(0))) v_taken_signal <- replicateM (mkBypassFIFOF);

   rule rl_deq;
      po.deq ();
      for (Integer j = 0; j < valueOf (n); j = j + 1)
	 v_taken_signal [j].deq ();
   endrule

   function PipeOut #(a) gen_fn (Integer j) = interface PipeOut;
						 method a first () if (v_taken_signal [j].notFull);
						    return po.first;
						 endmethod
						 method Action deq ();
						    v_taken_signal [j].enq (?);
						 endmethod
						 method Bool notEmpty ();
						    return po.notEmpty () && v_taken_signal [j].notFull;
						 endmethod
					      endinterface;
   return genWith (gen_fn);
endmodule: mkForkVector

// ----------------------------------------------------------------
// mkExplodeVector
// Explode an input stream of vectors into a vector of output streams

module mkExplodeVector
                #(PipeOut #(Vector #(n, a)) po)
                (Vector #(n, PipeOut #(a)));

   Vector #(n, FIFO #(Bit #(0))) v_taken_signal <- replicateM (mkBypassFIFO);

   rule rl_deq;
      po.deq ();
      for (Integer j = 0; j < valueOf (n); j = j + 1)
	 v_taken_signal [j].deq ();
   endrule

   function PipeOut #(a) gen_fn (Integer j) = interface PipeOut;
						 method a first ();
						    return po.first [j];
						 endmethod
						 method Action deq ();
						    v_taken_signal [j].enq (?);
						 endmethod
						 method Bool notEmpty ();
						    return po.notEmpty ();
						 endmethod
					      endinterface;
   return genWith (gen_fn);
endmodule: mkExplodeVector

// ----------------------------------------------------------------
// mkForkAndBufferRight
// Forks an input into two, and buffers one of them
// I.e., allows a copy of the data to be used in current stage (left, unbuffered)
//       and also forwarded to the next stage (right, buffered),
//       a common structure in pipelines

module mkForkAndBufferRight
   #(PipeOut #(a) poa)
   (Tuple2 #(PipeOut #(a), PipeOut #(a)))
   provisos (Bits #(a, sa));

   Vector #(2, PipeOut #(a)) forked <- mkForkVector (poa);
   PipeOut #(a)              right  <- mkBuffer (forked [1]);
   return tuple2 (forked [0], right);
endmodule

// ----------------------------------------------------------------
// mkJoin
// Join two input streams into one output stream
//     join_fn is used to combine two input items (va,vb) into an output item vc

module mkJoin
                #(function c join_fn (a va, b vb),
                  PipeOut #(a) poa,
                  PipeOut #(b) pob)
                (PipeOut #(c));

   method c first ();
      return join_fn (poa.first (), pob.first ());
   endmethod

   method Action deq ();
      poa.deq ();
      pob.deq ();
   endmethod

   method Bool notEmpty ();
      return (poa.notEmpty () && pob.notEmpty ());
   endmethod
endmodule: mkJoin

// ****************************************************************
// **************** LINEAR PIPES
// ****************************************************************

// ----------------------------------------------------------------
// Compose two pipes into a single pipe

module [Module] mkCompose
                #(Pipe #(a, b) pab,
                  Pipe #(b, c) pbc,
                  PipeOut #(a) pa)
                (PipeOut #(c));

   PipeOut #(b) pipeAtoB <- pab (pa);
   PipeOut #(c) pipeBtoC <- pbc (pipeAtoB);
   return pipeBtoC;
endmodule

// ----------------------------------------------------------------
// Compose two pipes into a single pipe
//     param_with_buffer specifies whether there should be an intervening buffer

module [Module] mkCompose_buffered
                #(Bool param_with_buffer,
                  Pipe #(a, b) pab,
                  Pipe #(b, c) pbc,
                  PipeOut #(a) pa)
                (PipeOut #(c))
        provisos (Bits #(b, sb));

   PipeOut #(b) pipeAtoB <- pab (pa);
   let composeBuffer = pipeAtoB;

   if (param_with_buffer)
      composeBuffer <- mkBuffer (pipeAtoB);

   PipeOut #(c) pipeBtoC <- pbc (composeBuffer);
   return pipeBtoC;
endmodule

// ----------------------------------------------------------------
// Build a linear pipeline of n stages (_S is for 'static index')
// The stage index (0..n-1) is a static input (parameter) to mkStage()

module [Module] mkLinearPipe_S
                #(Integer n,
                  function Pipe #(a,a) mkStage (UInt #(logn) j),
                  PipeOut #(a) po_in)
                (PipeOut #(a));

   if (n < 1)
      errorM ("mkLinearPipe_S: iteration count parameter < 1: " + integerToString (n));

   if (n >= (2 ** valueof (logn)))
      errorM ("mkLinearPipe_S: iteration count (" + integerToString (n) +
              ") will not fit in available bits: " + integerToString (valueof (logn)));

   PipeOut #(a) linearPipeStage = po_in;

   for (Integer j = 0; j < n; j = j + 1)
      linearPipeStage <- mkStage (fromInteger (j), linearPipeStage);

   return linearPipeStage;
endmodule: mkLinearPipe_S

// ----------------------------------------------------------------
// An alternate version of the linear pipe, where the arguments types
// match those for mkForLoop

module [Module] mkLinearPipe_S_Alt
                #(Integer       jmax,
                  Pipe #(Tuple2 #(a, UInt #(wj)), Tuple2 #(a, UInt #(wj)))    mkStage,
                  Pipe #(a,b)    mkFinal,
                  PipeOut #(a)   po_in)
                (PipeOut #(b))
        provisos (Bits #(a, sa));

   function Tuple2 #(a, UInt #(wj)) mkInitial(a va) = tuple2(va, 0);
   function a mkLast(Tuple2 #(a, UInt #(wj)) va) = tpl_1(va);

   PipeOut #(Tuple2 #(a, UInt #(wj))) linearPipeStage <- mkFn_to_Pipe(mkInitial, po_in);

   function Tuple2 #(a, UInt #(wj)) mkBody(Tuple2 #(a, UInt #(wj)) va) = tuple2(tpl_1(va), tpl_2(va) + 1);

   linearPipeStage <- mkStage (linearPipeStage);
   linearPipeStage <- mkFn_to_Pipe(mkBody, linearPipeStage);
   for (Integer  j = 0; j < jmax; j = j + 1) begin
      linearPipeStage <- mkStage (linearPipeStage);
      linearPipeStage <- mkFn_to_Pipe(mkBody, linearPipeStage);
   end
   PipeOut#(a) last <- mkFn_to_Pipe(mkLast, linearPipeStage);
   PipeOut#(b) finalStage <- mkFinal(last);

   return finalStage;
endmodule: mkLinearPipe_S_Alt

// ----------------------------------------------------------------
// Build a linear pipeline of n stages (_D is for 'dynamic index')
// The stage index (0..n-1) is a dynamic input to mkStage()
// mkStage takes a 2-tuple (j,x) as input, where j is the stage index and x is data
// TODO: NOT YET TESTED

module [Module] mkLinearPipe_D
                #(Integer n,
                  function Pipe #(Tuple2 #(a, UInt #(logn)), a) mkStage (),
                  PipeOut #(a) po)
                (PipeOut #(a));

   if (n < 0)
      errorM ("mkLinearPipe_D: iteration count parameter < 0: " + integerToString (n));

   if (n >= (2 ** valueof (logn)))
      errorM ("mkLinearPipe_D: iteration count (" + integerToString (n) +
              ") will not fit in available bits: " + integerToString (valueof (logn)));

   PipeOut #(a) linearPipeStage = po;

   for (Integer j = 0; j < n; j = j + 1) begin

      PipeOut #(UInt #(logn)) po_const_j <- mkSource_from_constant (fromInteger (j));

      PipeOut #(Tuple2 #(a, UInt #(logn))) po_tup <- mkJoin (tuple2, linearPipeStage, po_const_j);

      linearPipeStage <- mkStage (po_tup);
   end

   return linearPipeStage;
endmodule: mkLinearPipe_D

// ****************************************************************
// **************** CONDITIONAL PIPES
// ****************************************************************

// ----------------------------------------------------------------
// mkIfThenElse
// Stream args 'xs' through either 'pipeT' or 'pipeF' depending on booleans 'bs'
// The arms pipeT and pipeF need not have the same latency; ordering is preserved
//     using the internal pipe 'bools'
// For full pipelinining, the 'latency' parameter should be the max of latencies of pipeT and pipeF

module [Module] mkIfThenElse
                #(Integer latency,
                  Pipe #(a,b)  pipeT,
                  Pipe #(a,b)  pipeF,
                  PipeOut #(Tuple2 #(a, Bool)) poa)
                (PipeOut #(b));

   FIFOF #(Bool) bools <- mkSizedFIFOF_local (latency);

   match { .x, .pred } = poa.first ();

   PipeOut #(a) po_t_in = interface PipeOut;
                             method a first () if (pred);
                                return x;
                             endmethod
                             method Action deq () if (pred);
                                poa.deq ();
                                bools.enq (True);
                             endmethod
                             method Bool notEmpty ();
                                return ( poa.notEmpty () ? pred : False );
                             endmethod
                          endinterface;
   PipeOut #(b) ifThenElse_TruePipe <- pipeT (po_t_in);

   PipeOut #(a) po_f_in = interface PipeOut;
                             method a first () if (! pred);
                                return x;
                             endmethod
                             method Action deq () if (! pred);
                                poa.deq ();
                                bools.enq (False);
                             endmethod
                             method Bool notEmpty ();
                                return ( poa.notEmpty () ? (! pred) : False );
                             endmethod
                          endinterface;
   PipeOut #(b) ifThenElse_FalsePipe <- pipeF (po_f_in);

   method b first ();
      return (bools.first () ? ifThenElse_TruePipe.first () : ifThenElse_FalsePipe.first ());
   endmethod

   method Action deq ();
      if (bools.first ())
         ifThenElse_TruePipe.deq ();
      else
         ifThenElse_FalsePipe.deq ();
      bools.deq ();
   endmethod

   method Bool notEmpty ();
      return (   bools.notEmpty ()
              ?  (   bools.first ()
                  ?  ifThenElse_TruePipe.notEmpty ()
                  :  ifThenElse_FalsePipe.notEmpty () )
              :  False );
   endmethod
endmodule: mkIfThenElse

// ----------------------------------------------------------------
// mkIfThenElse_unordered
// Stream args 'xs' through either 'pipeT' or 'pipeF' depending on booleans 'bs'
// The arms pipeT and pipeF need not have the same latency
// The pipes are drained using a round-robin scheduler

module [Module] mkIfThenElse_unordered
                #(Pipe #(a,b)  pipeT,
                  Pipe #(a,b)  pipeF,
                  PipeOut #(Tuple2 #(a, Bool)) poa)
                (PipeOut #(b));

   match { .x, .pred } = poa.first ();

   // ---- True side
   PipeOut #(a) po_t_in = interface PipeOut;
                             method a first () if (pred);
                                return x;
                             endmethod
                             method Action deq () if (pred);
                                poa.deq ();
                             endmethod
                             method Bool notEmpty ();
                                return ( poa.notEmpty () ? pred : False );
                             endmethod
                          endinterface;
   PipeOut #(b) ifThenElse_TruePipe <- pipeT (po_t_in);

   // ---- False side
   PipeOut #(a) po_f_in = interface PipeOut;
                             method a first () if (! pred);
                                return x;
                             endmethod
                             method Action deq () if (! pred);
                                poa.deq ();
                             endmethod
                             method Bool notEmpty ();
                                return ( poa.notEmpty () ? (! pred) : False );
                             endmethod
                          endinterface;
   PipeOut #(b) ifThenElse_FalsePipe <- pipeF (po_f_in);

   // ---- Round robin scheduling
   Reg #(Bool) round_robin <- mkReg (False);

   Bool both_ready = (ifThenElse_TruePipe.notEmpty && ifThenElse_FalsePipe.notEmpty);
   Bool choose_t = ( both_ready ? round_robin : ifThenElse_TruePipe.notEmpty );

   // ---- Interface
   method b first ();
      return ( choose_t ? ifThenElse_TruePipe.first () : ifThenElse_FalsePipe.first () );
   endmethod

   method Action deq ();
      if (choose_t)
         ifThenElse_TruePipe.deq ();
      else
         ifThenElse_FalsePipe.deq ();
   endmethod

   method Bool notEmpty ();
      return  (ifThenElse_TruePipe.notEmpty () || ifThenElse_FalsePipe.notEmpty ());
   endmethod
endmodule: mkIfThenElse_unordered

// ****************************************************************
// **************** LOOPS
// ****************************************************************

// ----------------------------------------------------------------
// mkWhileLoop

module [Module] mkWhileLoop
                #(Pipe #(a,Tuple2 #(b, Bool))  mkPreTest,
                  Pipe #(b,a)                  mkPostTest,
                  Pipe #(b,c)                  mkFinal,
                  PipeOut #(a) po_in)
                (PipeOut #(c))
        provisos (Bits #(a, sa));


   // --- At the top of the loop is the merge of incoming data and recirculated data
   FIFOF #(a) merge_fifo <- mkFIFOF;  // Note: this cannot be a pipelinefifo (else scheduling loop)

   // ---- Pre test pipeline
   let mkWhileLoop_preTestPipe <- mkPreTest (f_FIFOF_to_PipeOut (merge_fifo));

   match { .vb, .vBool } = mkWhileLoop_preTestPipe.first ();

   // ---- True output of test
   let po_t = interface PipeOut #(b)
                 method b first () if (vBool);
                    return vb;
                 endmethod
                 method Action deq () if (vBool);
                    mkWhileLoop_preTestPipe.deq ();
                 endmethod
                 method Bool notEmpty ();
                    return ( mkWhileLoop_preTestPipe.notEmpty () ? vBool : False);
                 endmethod
              endinterface;

   // ---- False output of test
   let po_f = interface PipeOut #(b)
                 method b first () if (! vBool);
                    return vb;
                 endmethod
                 method Action deq () if (! vBool);
                    mkWhileLoop_preTestPipe.deq ();
                 endmethod
                 method Bool notEmpty ();
                    return ( mkWhileLoop_preTestPipe.notEmpty () ? (! vBool) : False);
                 endmethod
              endinterface;

   // ---- post test loop body

   let mkWhileLoop_postTestPipe <- mkPostTest (po_t);

   // ---- Merge loop input or recirc, giving priority to recirc

   (* preempts = "rl_merge_loop_recirc, rl_merge_loop_input" *)
   rule rl_merge_loop_input;
      let x = po_in.first (); po_in.deq ();
      merge_fifo.enq (x);
   endrule

   rule rl_merge_loop_recirc;
      let x = mkWhileLoop_postTestPipe.first (); mkWhileLoop_postTestPipe.deq ();
      merge_fifo.enq (x);
   endrule

   // ---- loop exit with final pipe
   let mkWhileLoop_finalPipe <- mkFinal (po_f);
   return mkWhileLoop_finalPipe;
endmodule: mkWhileLoop

// ----------------------------------------------------------------
// mkForLoop

module [Module] mkForLoop_OLD
                #(UInt #(wj)     jmax,
                  Pipe #(Tuple2 #(a, UInt #(wj)), Tuple2 #(a, UInt #(wj)))    mkLoopBody,
                  Pipe #(a,b)    mkFinal,
                  PipeOut #(a)   po_in)
                (PipeOut #(b))
        provisos (Bits #(a, sa));

   // --- At the top of the loop is the merge of incoming data and recirculated data
   // Note: this cannot be a pipelinefifo (else scheduling loop)

   FIFOF #(Tuple3 #(a, UInt #(wj), Bool)) merge_fifo <- mkFIFOF;

   match { .vj, .j, .done } = merge_fifo.first ();

   // ---- True output of test
   let po_t = interface PipeOut #(Tuple2 #(a, UInt #(wj)));
                 method Tuple2 #(a, UInt #(wj))  first () if (! done);
                    return tuple2 (vj, j);
                 endmethod
                 method Action deq () if (! done);
                    merge_fifo.deq ();
                 endmethod
                 method Bool notEmpty ();
                    return ((! done) && merge_fifo.notEmpty ());
                 endmethod
              endinterface;

   let mkForLoop_BodyPipe <- mkLoopBody (po_t);    // post test loop body

   // ---- False output of test
   let po_f = interface PipeOut #(a)
                 method a first () if (done);
                    return vj;
                 endmethod
                 method Action deq () if (done);
                    merge_fifo.deq ();
                 endmethod
                 method Bool notEmpty ();
                    return (done && merge_fifo.notEmpty ());
                 endmethod
              endinterface;

   let mkForLoop_FinalPipe <- mkFinal (po_f);

   // ---- Merge loop input or recirc, giving priority to recirc

   (* preempts = "rl_merge_loop_recirc, rl_merge_loop_input" *)
   rule rl_merge_loop_input;
      let x = po_in.first (); po_in.deq ();
      let j = 0;
      let done = (j == jmax);
      merge_fifo.enq (tuple3 (x, j, done));
   endrule

   rule rl_merge_loop_recirc;
      match { .x, .j } = mkForLoop_BodyPipe.first (); mkForLoop_BodyPipe.deq ();
      let done = (j == jmax);
      merge_fifo.enq (tuple3 (x, j+1, done));
   endrule

   // ---- loop exit with final pipe
   return mkForLoop_FinalPipe;
endmodule: mkForLoop_OLD

module [Module] mkForLoop
                #(UInt#(wj)     jmax,
                  Pipe #(Tuple2 #(a, UInt #(wj)), Tuple2 #(a, UInt #(wj)))    mkLoopBody,
                  Pipe #(a,b)    mkFinal,
                  PipeOut #(a)   po_in)
                (PipeOut #(b))
        provisos (Bits #(a, sa)
                  ,Alias #(Tuple3 #(a, UInt #(wj),Bool), intPipe_t)
           );

   // --- At the top of the loop is the merge of incoming data and recirculated data
   // Note: this cannot be a pipelinefifo (else scheduling loop)
   FIFOF #(intPipe_t) merge_fifo <- mkFIFOF;

   match { .vj, .j, .done } = merge_fifo.first ();
   Wire#(Tuple2#(a,UInt#(wj))) staleData <- mkWire();
   PulseWire bodyExecuted_pw <- mkPulseWire;
   PulseWire continue_pw     <- mkPulseWire;
   PulseWire newData_pw      <- mkPulseWire;
   PulseWire final_pw        <- mkPulseWire;

   (*  preempts = "continueLoop, newData" *)
   rule continueLoop (!done);
      staleData <= tuple2(vj, j);
      continue_pw.send;
   endrule
   rule newData (True);
      staleData <= tuple2 (po_in.first, 0);
      newData_pw.send;
   endrule

   rule deqContinue (continue_pw && bodyExecuted_pw || final_pw);
      merge_fifo.deq();
   endrule
   rule deqNew (newData_pw && bodyExecuted_pw);
      po_in.deq();
   endrule

   // ---- Continue loop execution
   let po_t = interface PipeOut #(Tuple2 #(a, UInt #(wj)));
                 method Tuple2 #(a, UInt #(wj))  first ();
                    return staleData;
                 endmethod
                 method Action deq ();
                    bodyExecuted_pw.send ();
                 endmethod
                 method Bool notEmpty ();
                    return continue_pw || newData_pw;
                 endmethod
              endinterface;

   let mkForLoop_BodyPipe <- mkLoopBody (po_t);

   // ---- False output of test
   let po_f = interface PipeOut #(a)
                 method a first () if (done);
                    return vj;
                 endmethod
                 method Action deq () if (done);
                    final_pw.send;
                 endmethod
                 method Bool notEmpty ();
                    return (done && merge_fifo.notEmpty ());
                 endmethod
              endinterface;

   let mkForLoop_FinalPipe <- mkFinal (po_f);

   rule rl_merge_loop_recirc;
      match { .x, .j } = mkForLoop_BodyPipe.first (); mkForLoop_BodyPipe.deq ();
      let thisdone = (j >= jmax);
      merge_fifo.enq (tuple3 (x, j+1, thisdone));
   endrule

   // ---- loop exit with final pipe
   return mkForLoop_FinalPipe;
endmodule: mkForLoop

// ****************************************************************
// **************** FOLDS
// ****************************************************************

// ----------------------------------------------------------------
// mkWhileFold
// Input po_in is a stream of pairs: (value, End-of-group)
// Combines (accumulates) each group of items using pipe_combine
// Returns accumulated value

module [Module] mkWhileFold
                #(Pipe #(Tuple2 #(a,a), a)  mkCombine,
                  PipeOut #(Tuple2 #(a, Bool)) po_in)
                (PipeOut #(a))
        provisos (Bits #(a, sa));

   FIFO #(Bit #(0))  output_taken_fifo <- mkBypassFIFO;
   Reg #(Bool)       initialized       <- mkReg (False);

   rule rl_initialize (! initialized);
      output_taken_fifo.enq (?);
      initialized <= True;
   endrule

   FIFOF #(a)  accum_fifo  <- mkLFIFOF;
   Reg #(Bool) done_accum  <- mkReg(False);

   // Note: accum_fifo's first/deq SBR enq    (pipeline fifo)
   // In the recirc path, we're both dequeuing and enqueing
   // Because of the SBR, the deq and enq have to be in two rules
   // Thus recirc is split into two rules A and B communicating with an RWire
   // The pulse wires and rl_assert are used to ensure they fire together

   RWire #(a)  recirc_rw  <- mkRWire;
   PulseWire   pw_rl_A    <- mkPulseWire;
   PulseWire   pw_rl_B    <- mkPulseWire;

   match { .x_in,     .done_in }  = po_in.first ();
   let   x_accum                  = accum_fifo.first ();

   rule rl_first_input;
      accum_fifo.enq (x_in);
      done_accum <= done_in;
      output_taken_fifo.deq ();
      po_in.deq ();
   endrule

   PipeOut #(Tuple2 #(a,a)) recirc_ifc =
                   interface PipeOut
                      method Tuple2 #(a,a) first () if (! done_accum);
                         return tuple2 (x_in, x_accum);
                      endmethod
                      method Action deq () if (! done_accum);
                         po_in.deq ();
                         accum_fifo.deq ();
                         done_accum <= done_in;
                      endmethod
                      method Bool notEmpty ();
                         return ((! done_accum) && po_in.notEmpty() && accum_fifo.notEmpty ());
                      endmethod
                   endinterface;

   PipeOut #(a) combine_out <- mkCombine (recirc_ifc);

   rule rl_recirc_A;
      recirc_rw.wset (combine_out.first ());
      combine_out.deq ();
      pw_rl_A.send ();
   endrule

   (* preempts = "rl_first_input, rl_recirc_B" *)
   rule rl_recirc_B (recirc_rw.wget matches tagged Valid .x);
      accum_fifo.enq (x);
      pw_rl_B.send ();
   endrule

   rule rl_assert_A_and_B (pw_rl_A || pw_rl_B);
      if (! pw_rl_A)
         $display ("mkWhileFold: rl_recirc_B firing without rl_recirc_A");
      if (! pw_rl_B)
         $display ("mkWhileFold: rl_recirc_A firing without rl_recirc_B");
   endrule

   return (interface PipeOut;
              method a first () if (initialized && done_accum);
                 return x_accum;
              endmethod
              method Action deq () if (initialized && done_accum);
                 accum_fifo.deq ();
                 output_taken_fifo.enq (?);
              endmethod
              method Bool notEmpty ();
                 return (initialized && done_accum && accum_fifo.notEmpty ());
              endmethod
           endinterface);
endmodule: mkWhileFold

// ----------------------------------------------------------------
// mkForFold
// Input po_in is a stream of values in groups (logically indexed 0..jmax).
// Combines (accumulates) each group of items using pipe mkCombine.
// Returns accumulated values.

module [Module] mkForFold
                #(UInt #(wj) jmax,
                  Pipe #(Tuple2 #(a,a), a)  mkCombine,
                  PipeOut #(a) po_in)
                (PipeOut #(a))
        provisos (Bits #(a, sa));

   FIFO #(Bit #(0))  output_taken_fifo <- mkBypassFIFO;
   Reg #(Bool)       initialized       <- mkReg (False);

   rule rl_initialize (! initialized);
      output_taken_fifo.enq (?);
      initialized <= True;
   endrule

   FIFOF #(a)         accum_fifo  <- mkLFIFOF;
   Reg #(UInt #(wj))  j           <- mkRegU;

   // Note: accum_fifo's first/deq SBR enq    (pipeline fifo)
   // In the recirc path, we're both dequeuing and enqueing
   // Because of the SBR, the deq and enq have to be in two rules
   // Thus recirc is split into two rules A and B communicating with an RWire
   // The pulse wires and rl_assert are used to ensure they fire together

   RWire #(a)                  recirc_rw  <- mkRWire;
   PulseWire                   pw_rl_A    <- mkPulseWire;
   PulseWire                   pw_rl_B    <- mkPulseWire;

   let x_in     = po_in.first ();
   let x_accum  = accum_fifo.first ();

   rule rl_first_input;
      accum_fifo.enq (x_in);
      j <= 0;
      output_taken_fifo.deq ();
      po_in.deq ();
   endrule

   PipeOut #(Tuple2 #(a,a)) recirc_ifc =
                   interface PipeOut
                      method Tuple2 #(a,a) first () if (j < jmax);
                         return tuple2 (x_in, x_accum);
                      endmethod
                      method Action deq () if (j < jmax);
                         po_in.deq ();
                         accum_fifo.deq ();
                      endmethod
                      method Bool notEmpty ();
                         return ((j < jmax) && po_in.notEmpty () && accum_fifo.notEmpty ());
                      endmethod
                   endinterface;

   PipeOut #(a) combine_out <- mkCombine (recirc_ifc);

   rule rl_recirc_A;
      recirc_rw.wset (combine_out.first ());
      combine_out.deq ();
      pw_rl_A.send ();
   endrule

   (* preempts = "rl_first_input, rl_recirc_B" *)
   rule rl_recirc_B (recirc_rw.wget matches tagged Valid .xb);
      accum_fifo.enq (xb);
      j <= j + 1;
      pw_rl_B.send ();
   endrule

   rule rl_assert_A_and_B (pw_rl_A || pw_rl_B);
      if (! pw_rl_A)
         $display ("mkForFold: rl_recirc_B firing without rl_recirc_A");
      if (! pw_rl_B)
         $display ("mkForFold: rl_recirc_A firing without rl_recirc_B");
   endrule

   return (interface PipeOut;
              method a first () if (initialized && (j == jmax));
                 return x_accum;
              endmethod
              method Action deq () if (initialized && (j == jmax));
                 accum_fifo.deq ();
                 output_taken_fifo.enq (?);
              endmethod
              method Bool notEmpty ();
                 return (initialized && (j == jmax) && accum_fifo.notEmpty ());
              endmethod
           endinterface);
endmodule: mkForFold

// ****************************************************************
// **************** Reordering
// ****************************************************************

// mkReorder restores I/O ordering for a pipe that returns results out of order
// mkBody takes inputs (tag,xa) and returns results (tag,xb), possibly out of order
// This module creates a Pipe#(a,b) that returns outputs in the order of the inputs

module [Module] mkReorder
                #(Pipe #(Tuple2 #(CBToken #(n), a), Tuple2 #(CBToken #(n), b)) mkBody,
                  PipeOut #(a) po_in)
                (PipeOut #(b))

        provisos (Bits #(a, sa),
                  Bits #(b, sb));

   CompletionBuffer #(n, b) cb <- mkCompletionBuffer;

   FIFOF #(Tuple2 #(CBToken #(n), a)) tagged_inputs <- mkLFIFOF;
   FIFOF #(b)                         outputs       <- mkLFIFOF;

   rule rl_tag_inputs;
      let x      =  po_in.first ();  po_in.deq ();
      let cbtok <-  cb.reserve.get ();
      tagged_inputs.enq (tuple2 (cbtok, x));
   endrule

   let body_out <- mkBody (f_FIFOF_to_PipeOut (tagged_inputs));

   rule rl_drain_body;
      cb.complete.put (body_out.first ());  body_out.deq ();
   endrule

   (* descending_urgency = "rl_drain_cb, rl_tag_inputs" *)

   rule rl_drain_cb;
      let z <- cb.drain.get ();
      outputs.enq (z);
   endrule

   return f_FIFOF_to_PipeOut (outputs);
endmodule: mkReorder

// ****************************************************************
// Repeated function application with same data
// ****************************************************************
// This is a model of Pipe#(a,b)
module [Module] mkReplicateFn #(UInt#(n) apply_count
                                ,function b fn (a x, UInt#(n) cnt)
                                ,PipeOut #(a) po_in
                                ) (PipeOut #(b));

   Reg#(UInt#(n)) cntr    <- mkReg(0);

   method b first ();
      return fn (po_in.first, cntr);
   endmethod
   method Action deq ();
      if (cntr == apply_count-1) begin
         cntr <= 0;
         po_in.deq ;
      end
      else begin
         cntr <= cntr + 1;
      end
   endmethod
   method Bool notEmpty();
      return po_in.notEmpty;
   endmethod
endmodule

// ****************************************************************
// Homogeneous Pipelined Vector tree reduction
// ****************************************************************
//  Use a type class to keep names the same.
typeclass VectorTreeReduce #( numeric type n, type a );
   module [Module] mkTreeReducePipe (Pipe#(Tuple2#(a,a), a) reducepipe
                                     ,Bit#(32)  addBuffer
                                     ,PipeOut#(Vector#(n,a)) pipein
                                     ,PipeOut#(a) ifc
                                     );
   module [Module] mkTreeReduceFn ( function a reduce2 (a x, a y)
                                   ,function a reduce1 (a x)
                                   ,Bit#(32)  addBuffer
                                   ,PipeOut#(Vector#(n,a)) pipein
                                   ,PipeOut#(a) ifc
                                   );
endtypeclass

instance VectorTreeReduce #(1, a)
   provisos ( Bits#(a,sa) );

   module [Module] mkTreeReducePipe (Pipe#(Tuple2#(a,a), a) reducepipe
                                     ,Bit#(32)  addBuffer
                                     ,PipeOut#(Vector#(1,a)) pipein
                                     ,PipeOut#(a) ifc
                                     );
      (*hide*)
      let _vreduce1 <- mkFn_to_Pipe (head,pipein);
      return _vreduce1;
   endmodule
   module [Module] mkTreeReduceFn ( function a reduce2 (a x, a y)
                                   ,function a reduce1 (a x)
                                   ,Bit#(32)  addBuffer
                                   ,PipeOut#(Vector#(1,a)) pipein
                                   ,PipeOut#(a) ifc
                                   )
      provisos ( Bits#(a,sa)
                );
      (*hide*)
      let _vTreeReduce1 <- mkFn_to_Pipe (head,pipein);
      return _vTreeReduce1;
   endmodule
endinstance


instance VectorTreeReduce #(2, a)
   provisos ( Bits#(a,sa) );
   module [Module] mkTreeReducePipe (Pipe#(Tuple2#(a,a), a) reducepipe
                                     ,Bit#(32)  addBuffer
                                     ,PipeOut#(Vector#(2,a)) pipein
                                     ,PipeOut#(a) ifc
                                     );
      let err = error("Impossible error in Tree reduction 2");
      (*hide*)
      PipeOut#(Tuple2#(a,a)) _paired <- mkFn_to_Pipe(compose(head, mapPairs(tuple2, err)), pipein);
      PipeOut#(a)            map2to1 <- reducepipe(_paired);
      let buffer1 = map2to1;

      if (addBuffer[0] == 1) begin
         buffer1 <- mkBuffer(buffer1);
      end

      return buffer1;
   endmodule
   module [Module] mkTreeReduceFn ( function a reduce2 (a x, a y)
                                   ,function a reduce1 (a x)
                                   ,Bit#(32)  addBuffer
                                   ,PipeOut#(Vector#(2,a)) pipein
                                   ,PipeOut#(a) ifc
                                   );
      let vTreeReduceFinal <- mkFn_to_Pipe_Buffered (False
                                                     ,compose(head, (mapPairs (reduce2, reduce1)))
                                                     ,addBuffer[0] == 1
                                                     ,pipein);
      return vTreeReduceFinal;
   endmodule
endinstance

instance VectorTreeReduce #(n, a)
   provisos (Bits#(a,sa)
             ,Div#(n,2,n2)
             ,VectorTreeReduce #(n2,a)
             );
   module [Module] mkTreeReducePipe (Pipe#(Tuple2#(a,a), a) reducepipe
                                     ,Bit#(32)  addBuffer
                                     ,PipeOut#(Vector#(n,a)) pipein
                                     ,PipeOut#(a) ifc
                                     );
      let err = error("Error in mkTreeReducePipe -- size of input pipe vector must be a power of 2");
      (*hide*)
      let _paired <- mkFn_to_Pipe(mapPairs(tuple2, err), pipein);
      let mapN2 <- mkMap (reducepipe, _paired);
      let bufferN2 = mapN2;

      if (addBuffer[0] == 1) begin
         bufferN2 <- mkBuffer(bufferN2);
      end

      let vTreeReduceN2 <- mkTreeReducePipe(reducepipe, addBuffer>>1, bufferN2);
      return vTreeReduceN2;
   endmodule
   module [Module] mkTreeReduceFn ( function a reduce2 (a x, a y)
                                   ,function a reduce1 (a x)
                                   ,Bit#(32)  addBuffer
                                   ,PipeOut#(Vector#(n,a)) pipein
                                   ,PipeOut#(a) ifc
                                   );
      PipeOut#(Vector#(n2,a)) reduceFnNtoN2 <- mkFn_to_Pipe_Buffered(False
                                                                     ,mapPairs (reduce2, reduce1)
                                                                     ,addBuffer[0] == 1
                                                                     ,pipein);
      PipeOut#(a) vTreeReduceN2 <- mkTreeReduceFn(reduce2, reduce1, (addBuffer >> 1), reduceFnNtoN2);
      return vTreeReduceN2;
   endmodule
endinstance

// ****************************************************************
// ****************************************************************

endpackage: PAClib
