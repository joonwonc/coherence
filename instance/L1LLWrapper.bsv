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

import L1LLSimple::*;

// from the Hemiola integration library
import HMemBank::*;
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

(* synthesize *)
module mkCCL1LL(CC);
    L1LLSimpleRss mem <- mkL1LLSimple();

    // from HCCTest.bsv
    let getRqId = 6'b000000;
    let setRqId = 6'b001000;

    // from HMembank.bsv
    CCMsgId readRqId = 6'h2; // [mesiRqS] in Hemiola
    CCMsgId invWRqId = 6'h15; // [mesiInvWRq] in Hemiola

    let mrqs = interface FIFOF#(CCMsg);
                   method Action enq(CCMsg mrq) = noAction;
                   method Action deq = mem.l1ll.to_mem.toM.deq;
                   method CCMsg first;
                       let tmm = mem.l1ll.to_mem.toM.first();
                       let cmsg = ?;
                       if (tmm matches tagged Ld .lrq)
                           cmsg = CCMsg {id: readRqId,
                                         type_: False,
                                         addr: lrq.addr,
                                         value: ? };
                       else if (tmm matches tagged Wb .wrs)
                           cmsg = CCMsg {id: invWRqId,
                                         type_: False,
                                         addr: wrs.addr,
                                         value: wrs.data };
                       return cmsg;
                   endmethod
                   method Action clear = noAction;
                   method Bool notFull; return False; endmethod
                   method Bool notEmpty = mem.l1ll.to_mem.toM.notEmpty;
               endinterface;
    let mrss = interface FIFOF#(CCMsg);
                   method Action enq(CCMsg mrs);
                       // XXX: keep track of id
                       let mrm = MemRsMsg {data: mrs.value, child: ?, id: unpack(0)};
                       mem.l1ll.to_mem.rsFromM.enq(mrm);
                   endmethod
                   method Action deq = noAction;
                   method CCMsg first; return unpack(0); endmethod
                   method Action clear = noAction;
                   method Bool notFull = mem.l1ll.to_mem.rsFromM.notFull;
                   method Bool notEmpty; return False; endmethod
               endinterface;

    MemBank mb <- mkMemBankBramA(mrqs, mrss);

    function ProcRq#(ProcRqId) fromCCMsg (CCMsg rq);
        let prq = ProcRq { id: 0, // XXX: keep track of id
                          addr: rq.addr,
                          toState: (rq.id == getRqId ? S : M),
                          op: (rq.id == getRqId ? Ld : St),
                          byteEn: ?,
                          data: rq.value[0], // XXX: need to know the actual line index
                          amoInst: ? };
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
    for (Integet i = 0; i < valueOf(l1DNum); i = i+1) begin
        function Action mem_enq_rq (CCMsg rq);
            return action
                       $display ("*** requested: %d ty(%d) addr(%x)", i, rq.type_, rq.addr);
                       mem.l1ll.dReq[i].req(fromCCMsg(rq));
                   endaction;
        endfunction;
        function ActionValue#(CCMsg) mem_deq_rs();
            return actionvalue
                       mem.rsDeqs[i].deq;
                       return mem.rsDeqs[i].first;
                   endactionvalue;
        endfunction;
        _l1Ifc[i] = getMemRqRs(mem_enq_rq, mem_deq_rs);
    end

    method Bool isInit();
        return True;
    endmethod
endmodule
