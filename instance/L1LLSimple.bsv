import Vector::*;
import Connectable::*;

// from the processor library
import Types::*;
import CCTypes::*;
import CCSizes::*;

// from the cache library
import CrossBar::*;
import L1Bank::*;
import L1Wrapper::*;
import LLBank::*;
import LLWrapper::*;
import L1LL::*;

typedef 8 L1DNum;
typedef 1 LLNum;

interface L1LLSimple;
    interface Vector#(L1DNum, L1ProcReq#(ProcRqId)) dReq;
    interface MemFifoClient#(LdMemRqId#(LLCRqMshrIdx), void) to_mem;
endinterface

// A two-level cache structure (L1-LL), without instruction caches, without DMA.
module mkL1LLSimple#(Vector#(L1DNum, L1ProcResp#(ProcRqId)) procResp)(L1LLSimple);
    Vector#(L1DNum, L1CacheWrapper) dc = ?;
    for(Integer i = 0; i < valueof(L1DNum); i = i+1) begin
        dc[i] <- mkL1CacheWrapper(procResp[i]);
    end
    LLBankWrapper llc <- mkLLBankWrapper();

    let cRqXBar <- mkL1CRqToLLXBar;
    let cRsXBar <- mkL1CRsToLLXBar;
    let pXBar <- mkLLPRqRsToL1XBar;

    for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
        mkConnection(cRqXBar.srcIfc[i], dc[i].to_parent.rqToP);
        mkConnection(cRsXBar.srcIfc[i], dc[i].to_parent.rsToP);
        mkConnection(pXBar.dstIfc[i], dc[i].to_parent.fromP);
    end

    mkConnection(cRqXBar.dstIfc[0], llc.to_child.rqFromC);
    mkConnection(cRsXBar.dstIfc[0], llc.to_child.rsFromC);
    mkConnection(pXBar.srcIfc[0], llc.to_child.toC);

    function L1ProcReq#(ProcRqId) getDReqIfc(L1CacheWrapper ifc);
        return ifc.procReq;
    endfunction
    interface dReq = map(getDReqIfc, dc);
    interface to_mem = llc.to_mem;
endmodule

(* synthesize *)
module mkL1LLI(L1LLSimple);
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
    return cc;
endmodule
