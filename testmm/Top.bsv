// bsv libraries
import SpecialFIFOs::*;
import Vector::*;
import StmtFSM::*;
import FIFO::*;
import Connectable::*;

// portz libraries
import Directory::*;
import CtrlMux::*;
import Portal::*;
import Leds::*;
import PortalMemory::*;
import MemServer::*;
import PortalMemory::*;
import Dma::*;
import DmaUtils::*;
import RbmTypes::*;

// generated by tool
import DmaConfigWrapper::*;
import DmaIndicationProxy::*;
import MmIndicationProxy::*;
import MmRequestWrapper::*;
import TimerIndicationProxy::*;
import TimerRequestWrapper::*;
import MmDebugIndicationProxy::*;

// defined by user
import Matrix::*;

//typedef TAdd#(K,J) NumMasters;
`ifdef PCIE
typedef 1 NumMasters;
`else
typedef 1 NumMasters;
`endif

typedef TMul#(TDiv#(TAdd#(K,J),NumMasters),NumMasters)  NumReadBuffers;
typedef TMul#(TDiv#(J,NumMasters),NumMasters)  NumWriteBuffers;

typedef 1                                        ReadClientFanout;
typedef TDiv#(NumReadBuffers,ReadClientFanout)   NumReadClients;
typedef 1                                        WriteClientFanout;
typedef TDiv#(NumWriteBuffers,WriteClientFanout) NumWriteClients;

module [Module] mkPortalTop(PortalTop#(addrWidth,TMul#(32,N),Empty,NumMasters))
   provisos (Add#(a__, addrWidth, 40),
	     Add#(a__, b__, 40),
	     Add#(addrWidth, c__, 52),
	     Add#(addrWidth, d__, 64),
	     Add#(e__, f__, 40),
	     Add#(f__, 12, b__),
	     Add#(b__, g__, 44)
	     );

   DmaIndicationProxy dmaIndicationProxy <- mkDmaIndicationProxy(DmaIndicationPortal);

   MmDebugIndicationProxy mmDebugIndicationProxy <- mkMmDebugIndicationProxy(MmDebugIndicationPortal);
   MmIndicationProxy mmIndicationProxy <- mkMmIndicationProxy(MmIndicationPortal);
   TimerIndicationProxy timerIndicationProxy <- mkTimerIndicationProxy(TimerIndicationPortal);
   Mm#(N) mm <- mkMm(mmIndicationProxy.ifc, timerIndicationProxy.ifc, mmDebugIndicationProxy.ifc);
   MmRequestWrapper mmRequestWrapper <- mkMmRequestWrapper(MmRequestPortal,mm.mmRequest);
   TimerRequestWrapper timerRequestWrapper <- mkTimerRequestWrapper(TimerRequestPortal,mm.timerRequest);
   
   Vector#(NumReadBuffers, DmaReadBuffer#(TMul#(32,N),BurstLen)) read_buffers <- replicateM(mkDmaReadBuffer);
   zipWithM(mkConnection, mm.readClients, map(ors, read_buffers));
   let readBufferClients = map(orc, read_buffers);

   Vector#(NumReadClients,ObjectReadClient#(TMul#(32,N))) readClients;
   if (valueOf(NumReadClients) < valueOf(NumReadBuffers)) begin
      module mkFanoutReadClients#(Integer i)(ObjectReadClient#(TMul#(32,N)));
	 Vector#(ReadClientFanout, ObjectReadClient#(TMul#(32,N))) subReadClients = takeAt(i*valueOf(ReadClientFanout), readBufferClients);
	 let readMux <- mkDmaReadMux(subReadClients);
	 return readMux;
      endmodule
      readClients <- genWithM(mkFanoutReadClients);
   end
   else begin
      //readClients = take(readBufferClients);
      readClients = readBufferClients;
   end

   Vector#(NumWriteBuffers, DmaWriteBuffer#(TMul#(32,N),BurstLen)) write_buffers <- replicateM(mkDmaWriteBuffer);
   zipWithM(mkConnection, mm.writeClients, map(ows,takeAt(0,write_buffers)));
   let writeBufferClients = map(owc, write_buffers);

   Vector#(NumWriteClients,ObjectWriteClient#(TMul#(32,N))) writeClients;
   if (valueOf(NumWriteClients) < valueOf(NumWriteBuffers)) begin
      module mkFanoutWriteClients#(Integer i)(ObjectWriteClient#(TMul#(32,N)));
	 Vector#(WriteClientFanout, ObjectWriteClient#(TMul#(32,N))) subWriteClients = takeAt(i*valueOf(WriteClientFanout), writeBufferClients);
	 let writeMux <- mkDmaWriteMux(/* i*valueOf(WriteClientFanout), */ subWriteClients);
	 return writeMux;
      endmodule
      writeClients <- genWithM(mkFanoutWriteClients);
   end
   else begin
      //writeClients = take(writeBufferClients);
      writeClients = writeBufferClients;
   end

   Reg#(Bool) once <- mkReg(False);
   rule foobar if (!once);
      $display("read buffers %d read clients %d write buffers %d write clients %d", valueOf(NumReadBuffers), valueOf(NumReadClients), valueOf(NumWriteBuffers), valueOf(NumWriteClients));
      once <= True;
   endrule

   MemServer#(addrWidth, TMul#(32,N), NumMasters) dma <- mkMemServer(dmaIndicationProxy.ifc, readClients, writeClients);
   DmaConfigWrapper dmaConfigWrapper <- mkDmaConfigWrapper(DmaConfigPortal,dma.request);

   Vector#(7,StdPortal) portals;
   portals[0] = mmRequestWrapper.portalIfc;
   portals[1] = mmIndicationProxy.portalIfc; 
   portals[2] = dmaConfigWrapper.portalIfc;
   portals[3] = dmaIndicationProxy.portalIfc; 
   portals[4] = timerRequestWrapper.portalIfc;
   portals[5] = timerIndicationProxy.portalIfc; 
   portals[6] = mmDebugIndicationProxy.portalIfc;
   StdDirectory dir <- mkStdDirectory(portals);
   let ctrl_mux <- mkSlaveMux(dir,portals);
   
   interface interrupt = getInterruptVector(portals);
   interface slave = ctrl_mux;
   interface masters = dma.masters;

endmodule : mkPortalTop
