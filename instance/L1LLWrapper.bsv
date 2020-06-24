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
// import CrossBar::*;
// import L1Bank::*;
// import L1Wrapper::*;
// import LLBank::*;
// import LLWrapper::*;
// import L1LL::*;

import L1LLSimple::*;

// from the Hemiola integration library
import HMemBank::*;
import HCC::*;
import HCCTypes::*;

(* synthesize *)
module mkCCL1LL(CC);
    Vector#(L1DNum, FIFOF#(CCMsg)) rss <- replicateM(mkFIFOF());

    function L1ProcResp#(ProcRqId) getL1ProcResp(Integer i);
        return (interface L1ProcResp;
            method Action respLd(ProcRqId id, Data d);
                noAction;
            endmethod
            method Action respLrScAmo(ProcRqId id, Data d);
                noAction;
            endmethod
            method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(ProcRqId id);
                return unpack(0);
            endmethod
            method Action evict(LineAddr a);
                noAction;
            endmethod
        endinterface);
    endfunction

    let cc <- mkL1LLSimple(map(getL1ProcResp, genVector));

    let mrqs = interface FIFOF#(CCMsg);
                   method Action enq(CCMsg mrq) = noAction;
                   method Action deq = cc.to_mem.toM.deq;
                   method CCMsg first;
                       let tmm = cc.to_mem.toM.first();
                       // TODO: convert [tmm] to [CCMsg]
                       return unpack(0);
                   endmethod
                   method Action clear = noAction;
                   method Bool notFull; return False; endmethod
                   method Bool notEmpty = cc.to_mem.toM.notEmpty;
               endinterface;
    let mrss = interface FIFOF#(CCMsg);
                   method Action enq(CCMsg mrs);
                       let mrm = ?; // TODO: convert [mrq] to [MemRsMsg]
                       cc.to_mem.rsFromM.enq(mrm);
                   endmethod
                   method Action deq = noAction;
                   method CCMsg first; return unpack(0); endmethod
                   method Action clear = noAction;
                   method Bool notFull = cc.to_mem.rsFromM.notFull;
                   method Bool notEmpty; return False; endmethod
               endinterface;

    MemBank mb <- mkMemBankBramA(mrqs, mrss);

    // from HCCTest.bsv
    let getRqId = 6'b000000;
    let setRqId = 6'b001000;
    let getRsId = 6'b000001;
    let setRsId = 6'b001001;

    function ProcRq#(ProcRqId) fromCCMsg (CCMsg rq);
        let prq = ProcRq { id: 0, // does not matter here for performance check
                          addr: rq.addr,
                          toState: (rq.id == getRqId ? S : M),
                          op: (rq.id == getRqId ? Ld : St),
                          byteEn: ?,
                          data: rq.value,
                          amoInst: ? };
        return prq;
    endfunction

    // interface CC
    method Action mem_enq_rq_0 (CCMsg rq); cc.dReq[0].req(fromCCMsg(rq)); endmethod
    method mem_deq_rs_0 = actionvalue rss[0].deq; return rss[0].first; endactionvalue;
    method Action mem_enq_rq_1 (CCMsg rq); cc.dReq[1].req(fromCCMsg(rq)); endmethod
    method mem_deq_rs_1 = actionvalue rss[1].deq; return rss[1].first; endactionvalue;
    method Action mem_enq_rq_2 (CCMsg rq); cc.dReq[2].req(fromCCMsg(rq)); endmethod
    method mem_deq_rs_2 = actionvalue rss[2].deq; return rss[2].first; endactionvalue;
    method Action mem_enq_rq_3 (CCMsg rq); cc.dReq[3].req(fromCCMsg(rq)); endmethod
    method mem_deq_rs_3 = actionvalue rss[3].deq; return rss[3].first; endactionvalue;
    method Action mem_enq_rq_4 (CCMsg rq); cc.dReq[4].req(fromCCMsg(rq)); endmethod
    method mem_deq_rs_4 = actionvalue rss[4].deq; return rss[4].first; endactionvalue;
    method Action mem_enq_rq_5 (CCMsg rq); cc.dReq[5].req(fromCCMsg(rq)); endmethod
    method mem_deq_rs_5 = actionvalue rss[5].deq; return rss[5].first; endactionvalue;
    method Action mem_enq_rq_6 (CCMsg rq); cc.dReq[6].req(fromCCMsg(rq)); endmethod
    method mem_deq_rs_6 = actionvalue rss[6].deq; return rss[6].first; endactionvalue;
    method Action mem_enq_rq_7 (CCMsg rq); cc.dReq[7].req(fromCCMsg(rq)); endmethod
    method mem_deq_rs_7 = actionvalue rss[7].deq; return rss[7].first; endactionvalue;
    method Bool isInit(); return True; endmethod
endmodule
