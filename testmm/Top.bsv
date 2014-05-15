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

// defined by user
import Matrix::*;

//typedef TAdd#(K,J) NumMasters;
//typedef 1 NumMasters;
typedef 4 NumMasters;

typedef TMul#(TDiv#(TAdd#(K,J),NumMasters),NumMasters)  NumReadBuffers;
typedef TMul#(TDiv#(J,NumMasters),NumMasters)  NumWriteBuffers;

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

   MmIndicationProxy mmIndicationProxy <- mkMmIndicationProxy(MmIndicationPortal);
   TimerIndicationProxy timerIndicationProxy <- mkTimerIndicationProxy(TimerIndicationPortal);
   Mm#(N) mm <- mkMm(mmIndicationProxy.ifc, timerIndicationProxy.ifc);
   MmRequestWrapper mmRequestWrapper <- mkMmRequestWrapper(MmRequestPortal,mm.mmRequest);
   TimerRequestWrapper timerRequestWrapper <- mkTimerRequestWrapper(TimerRequestPortal,mm.timerRequest);
   
   Vector#(NumReadBuffers, DmaReadBuffer#(TMul#(32,N),BurstLen)) read_buffers <- replicateM(mkDmaReadBuffer);
   zipWithM(mkConnection, mm.readClients, map(ors, read_buffers));
   let readClients = map(orc, read_buffers);

   Vector#(NumWriteBuffers, DmaWriteBuffer#(TMul#(32,N),BurstLen)) write_buffers <- replicateM(mkDmaWriteBuffer);
   zipWithM(mkConnection, mm.writeClients, map(ows,takeAt(0,write_buffers)));
   let writeClients = map(owc, write_buffers);

   MemServer#(addrWidth, TMul#(32,N), NumMasters) dma <- mkMemServer(dmaIndicationProxy.ifc, readClients, writeClients);
   DmaConfigWrapper dmaConfigWrapper <- mkDmaConfigWrapper(DmaConfigPortal,dma.request);

   Vector#(6,StdPortal) portals;
   portals[0] = mmRequestWrapper.portalIfc;
   portals[1] = mmIndicationProxy.portalIfc; 
   portals[2] = dmaConfigWrapper.portalIfc;
   portals[3] = dmaIndicationProxy.portalIfc; 
   portals[4] = timerRequestWrapper.portalIfc;
   portals[5] = timerIndicationProxy.portalIfc; 
   StdDirectory dir <- mkStdDirectory(portals);
   let ctrl_mux <- mkSlaveMux(dir,portals);
   
   interface interrupt = getInterruptVector(portals);
   interface slave = ctrl_mux;
   interface masters = dma.masters;

endmodule : mkPortalTop
