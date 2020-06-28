import Vector::*;
import Connectable::*;
import FIFOF::*;

// from the processor library
import Types::*;
import MemoryTypes::*;
import CacheUtils::*;

// from the cache library
import CCTypes::*;
import CCSizes::*;
import DelayMemTypes::*;
import IdealDelayMem::*;

import L1LLSimple::*;

// from the Hemiola integration library
// import HMemBank::*;
import HCC::*;
import HCCTypes::*;

interface L1LLSimpleRss;
    interface L1LLSimple l1ll;
    interface Vector#(L1DNum, FifoDeq#(CCMsg)) rsDeqs;
endinterface

(* synthesize *)
module mkL1LLSimple(L1LLSimpleRss);
    Vector#(L1DNum, FIFOF#(CCMsg)) rss <- replicateM(mkFIFOF());

    // from HCCTest.bsv
    let getRsId = 6'b000001;
    let setRsId = 6'b001001;

    function L1ProcResp#(ProcRqId) getL1ProcResp(Integer i);
        return (interface L1ProcResp;
           method Action respLd(ProcRqId id, Data d);
               $display ("*** respLd: id(%d) data(%x)", id, d);
               // XXX: keep track of id to get the request address
               // XXX: response does not need to contain a *line*.
               let rline = replicate(0);
               rline = update(rline, 0, d);
               let rs = CCMsg {id: getRsId, type_: True, addr: ?, value: rline};
               rss[i].enq(rs);
           endmethod
           method Action respLrScAmo(ProcRqId id, Data d) = noAction;
           method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(ProcRqId id);
               $display ("*** respSt: id(%d)", id);
               // XXX: keep track of id to get the request address
               let rs = CCMsg {id: setRsId, type_: True, addr: ?, value: ?};
               rss[i].enq(rs);
               return unpack(0);
           endmethod
           method Action evict(LineAddr a) = noAction;
        endinterface);
    endfunction

    let cc <- mkL1LLSimpleA(map(getL1ProcResp, genVector));

    function FifoDeq#(t) toFifoDeq(FIFOF#(t) f);
        return (interface FifoDeq;
                    method notEmpty = f.notEmpty;
                    method deq = f.deq;
                    method first = f.first;
                endinterface);
    endfunction

    interface l1ll = cc;
    interface rsDeqs = map(toFifoDeq, rss);
endmodule

typedef 30 MemDelay;
typedef 18 LgTestMemSzBytes;

(* synthesize *)
module mkCCL1LL(CC);
    L1LLSimpleRss mem <- mkL1LLSimple();
    Reg#(Bool) memInit <- mkReg(False);
    Reg#(Bit#(64)) waitCount <- mkReg(0);

    IdealDelayMem#(MemDelay, LgTestMemSzBytes, LdMemRqId#(LLCRqMshrIdx), void) delayMem <- mkIdealDelayMem;
    mkConnection(mem.l1ll.to_mem, delayMem.to_proc);

    // Found from [../test/L1LL/TbL1LL.bsv]: need to wait for LLC to initialize all BRAMs
    rule init_mem (!memInit);
        waitCount <= waitCount + 1;
        if(waitCount == fromInteger(valueOf(TExp#(LLIndexSz)))) begin
            $display ("*** memory init done");
            memInit <= True;
        end
    endrule

    // from HCCTest.bsv
    let getRqId = 6'b000000;
    let setRqId = 6'b001000;

    function ProcRq#(ProcRqId) fromCCMsg (CCMsg rq);
        let prq = ProcRq { id: 0, // XXX: keep track of id
                          addr: rq.addr,
                          toState: (rq.id == getRqId ? S : M),
                          op: (rq.id == getRqId ? Ld : St),
                          byteEn: unpack(0),
                          data: rq.value[0], // XXX: need to know the actual line index
                          amoInst: unpack(0) };
        return prq;
    endfunction

    // interface CC
    function MemRqRs getMemRqRs (function Action enq_rq (CCMsg _),
                                 function ActionValue#(CCMsg) deq_rs ());
        return interface MemRqRs;
                   method mem_enq_rq = enq_rq;
                   method mem_deq_rs = deq_rs;
               endinterface;
    endfunction

    Vector#(L1DNum, MemRqRs) _l1Ifc = newVector();
    for (Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
        function Action mem_enq_rq (CCMsg rq);
            return action
                       $display ("*** requested: %d ty(%d) addr(%x)", i, rq.type_, rq.addr);
                       mem.l1ll.dReq[i].req(fromCCMsg(rq));
                   endaction;
        endfunction
        function ActionValue#(CCMsg) mem_deq_rs();
            return actionvalue
                       mem.rsDeqs[i].deq;
                       return mem.rsDeqs[i].first;
                   endactionvalue;
        endfunction
        _l1Ifc[i] = getMemRqRs(mem_enq_rq, mem_deq_rs);
    end
    interface l1Ifc = _l1Ifc;

    method Bool isInit();
        return memInit;
    endmethod
endmodule
