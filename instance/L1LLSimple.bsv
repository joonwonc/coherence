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

// Instantiations of some crossbars

typedef TExp#(LgLLBankNum) LLNum;
typedef Bit#(LgLLBankNum) LLIdx;
typedef Bit#(TLog#(L1Num)) L1Idx;

typedef CRqMsg#(L1Way, void) CRqFromL1;
typedef CRqMsg#(LLCRqId, LLChild) CRqToLL;
typedef CRsMsg#(void) CRsFromL1;
typedef CRsMsg#(LLChild) CRsToLL;
typedef PRqRsMsg#(LLCRqId, LLChild) PRqRsFromLL;
typedef PRqRsMsg#(L1Way, void) PRqRsToL1;

typedef 1 XBarSrcDelay;
typedef 2 XBarDstDelay;

// cross bar for L1 cRq to LL
typedef CrossBar#(L1Num, XBarSrcDelay, CRqFromL1, LLNum, XBarDstDelay, CRqToLL) L1CRqToLLXBar;

(* synthesize *)
module mkL1CRqToLLXBar(L1CRqToLLXBar);
    function XBarDstInfo#(LLIdx, CRqToLL) getL1CRqDstInfo(L1Idx whichL1, CRqFromL1 rq);
        return XBarDstInfo {
            idx: 0,
            data: CRqMsg {
                addr: rq.addr,
                fromState: rq.fromState,
                toState: rq.toState,
                canUpToE: rq.canUpToE,
                id: rq.id,
                child: whichL1
            }
        };
    endfunction

    let m <- mkCrossBar(getL1CRqDstInfo);
    return m;
endmodule

// cross bar for L1 cRs to LL
typedef CrossBar#(L1Num, XBarSrcDelay, CRsFromL1, LLNum, XBarDstDelay, CRsToLL) L1CRsToLLXBar;

(* synthesize *)
module mkL1CRsToLLXBar(L1CRsToLLXBar);
    function XBarDstInfo#(LLIdx, CRsToLL) getL1CRsDstInfo(L1Idx whichL1, CRsFromL1 rs);
        return XBarDstInfo {
            idx: 0,
            data: CRsMsg {
                addr: rs.addr,
                toState: rs.toState,
                data: rs.data,
                child: whichL1
            }
        };
    endfunction

    let m <- mkCrossBar(getL1CRsDstInfo);
    return m;
endmodule

// cross bar for LL pRqRs to L1
typedef CrossBar#(LLNum, XBarSrcDelay, PRqRsFromLL, L1Num, XBarDstDelay, PRqRsToL1) LLPRqRsToL1XBar;

(* synthesize *)
module mkLLPRqRsToL1XBar(LLPRqRsToL1XBar);
    function XBarDstInfo#(L1Idx, PRqRsToL1) getLLPRqRsDstInfo(LLIdx whichLL, PRqRsFromLL msg);
        return (case(msg) matches
            tagged PRq .rq: return XBarDstInfo {
                idx: rq.child,
                data: PRq (PRqMsg {
                    addr: rq.addr,
                    toState: rq.toState,
                    child: ?
                })
            };
            tagged PRs .rs: return XBarDstInfo {
                idx: rs.child,
                data: PRs (PRsMsg {
                    addr: rs.addr,
                    toState: rs.toState,
                    child: ?,
                    data: rs.data,
                    id: rs.id
                })
            };
        endcase);
    endfunction

    let m <- mkCrossBar(getLLPRqRsDstInfo);
    return m;
endmodule

////////// Now the simple L1LL module

interface L1LLSimple;
    interface Vector#(L1DNum, L1ProcReq#(ProcRqId)) dReq;
    interface MemFifoClient#(LdMemRqId#(LLCRqMshrIdx), void) to_mem;
endinterface

// A two-level cache structure (L1-LL), without instruction caches, without DMA.
module mkL1LLSimpleA#(Vector#(L1DNum, L1ProcResp#(ProcRqId)) procResp)(L1LLSimple);
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
