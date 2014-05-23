
/*
   /home/jamey/xbsv/scripts/importbvi.py
   -o
   FpAdd.bsv
   -c
   aclk
   -f
   s_axis_a
   -f
   s_axis_b
   -f
   m_axis_result
   -I
   FpAdd
   -P
   FpAdd
   fp_add_stub.v
*/

import Clocks::*;
import DefaultValue::*;
import XilinxCells::*;
import GetPut::*;

interface FpAdd;
   method ActionValue#(Bit#(32)) m_axis_result;
   method Action s_axis_a(Bit#(32) tdata);
    method Action s_axis_b(Bit#(32) tdata);
   method Action s_axis_operation(Bit#(8) v);
endinterface
import "BVI" fp_add =
module mkFpAdd(FpAdd);
   default_clock aclk(aclk);
   default_reset aresetn(aresetn);
   method m_axis_result_tdata m_axis_result() enable(m_axis_result_tready) ready(m_axis_result_tvalid);
   method s_axis_a(s_axis_a_tdata) enable(s_axis_a_tvalid) ready(s_axis_a_tready);
   method s_axis_b(s_axis_b_tdata) enable(s_axis_b_tvalid) ready(s_axis_b_tready);
   method s_axis_operation(s_axis_operation_tdata) enable(s_axis_operation_tvalid) ready(s_axis_operation_tready);
   schedule (m_axis_result, s_axis_a, s_axis_b, s_axis_operation) CF (m_axis_result, s_axis_a, s_axis_b, s_axis_operation);
endmodule
