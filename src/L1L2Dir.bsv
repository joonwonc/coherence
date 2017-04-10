import Types::*;
import CacheUtils::*;
import CCTypes::*;
import CCSizes::*;
import L1Wrapper::*;
import L2Wrapper::*;
import DirWrapper::*;
import CrossBar::*;
import Vector::*;
import GetPut::*;
import ClientServer::*;
import L1Bank::*;
import IntBank::*;
import DirBank::*;
import Connectable::*;

export L1L2Dir(..);
export mkL1L2Dir;

// XXX no banking: LgDirBankNum = 0, LgL2BankNum = 0, LgL1BankNum = 0

// cRq XBar
typedef CrossBar#(
    childNum, CRqMsg#(cRqIdT, void), 1, CRqMsg#(cRqIdT, Bit#(TLog#(childNum)))
) CRqXBar#(numeric type childNum, type cRqIdT);

module mkCRqXBar(CRqXBar#(childNum, cRqIdT)) provisos(
    Alias#(childT, Bit#(TLog#(childNum))),
    Alias#(cRqFromCT, CRqMsg#(cRqIdT, void)),
    Alias#(cRqToPT, CRqMsg#(cRqIdT, childT)),
    Bits#(cRqFromCT, a__),
    Bits#(cRqToPT, b__),
    FShow#(cRqToPT)
);
    function XBarDstInfo#(Bit#(0), cRqToPT) getCRqDstInfo(childT ch, cRqFromCT rq);
        return XBarDstInfo {
            idx: 0,
            data: CRqMsg {
                addr: rq.addr,
                fromState: rq.fromState,
                toState: rq.toState,
                id: rq.id,
                child: ch
            }
        };
    endfunction
    let m <- mkCrossBar(getCRqDstInfo);
    return m;
endmodule

// cRs XBar
typedef CrossBar#(
    childNum, CRsMsg#(void), 1, CRsMsg#(Bit#(TLog#(childNum)))
) CRsXBar#(numeric type childNum);

module mkCRsXBar(CRsXBar#(childNum)) provisos (
    Alias#(childT, Bit#(TLog#(childNum))),
    Alias#(cRsFromCT, CRsMsg#(void)),
    Alias#(cRsToPT, CRsMsg#(childT)),
    Bits#(cRsFromCT, a__),
    Bits#(cRsToPT, b__),
    FShow#(cRsToPT)
);
    function XBarDstInfo#(Bit#(0), cRsToPT) getCRsDstInfo(childT ch, cRsFromCT rs);
        return XBarDstInfo {
            idx: 0,
            data: CRsMsg {
                addr: rs.addr,
                toState: rs.toState,
                data: rs.data,
                child: ch
            }
        };
    endfunction
    let m <- mkCrossBar(getCRsDstInfo);
    return m;
endmodule

// pRqRs XBar
typedef CrossBar#(
    1, PRqRsMsg#(cRqIdT, Bit#(TLog#(childNum))), childNum, PRqRsMsg#(cRqIdT, void)
) PRqRsXBar#(numeric type childNum, type cRqIdT);

module mkPRqRsXBar(PRqRsXBar#(childNum, cRqIdT)) provisos (
    Alias#(childT, Bit#(TLog#(childNum))),
    Alias#(pRqRsFromPT, PRqRsMsg#(cRqIdT, childT)),
    Alias#(pRqRsToCT, PRqRsMsg#(cRqIdT, void)),
    Bits#(pRqRsFromPT, a__),
    Bits#(pRqRsToCT, b__),
    FShow#(pRqRsToCT)
);
    function XBarDstInfo#(childT, pRqRsToCT) getPRqRsDstInfo(Bit#(0) parent, pRqRsFromPT msg);
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
    let m <- mkCrossBar(getPRqRsDstInfo);
    return m;
endmodule

// L1 <-> L2
(* synthesize *)
module mkL1CRqToL2XBar(CRqXBar#(L2ChildNum, L1Way));
    let m <- mkCRqXBar;
    return m;
endmodule
(* synthesize *)
module mkL1CRsToL2XBar(CRsXBar#(L2ChildNum));
    let m <- mkCRsXBar;
    return m;
endmodule
(* synthesize *)
module mkL2PRqRsToL1XBar(PRqRsXBar#(L2ChildNum, L1Way));
    let m <- mkPRqRsXBar;
    return m;
endmodule

// L2 <-> Dir
(* synthesize *)
module mkL2CRqToDirXBar(CRqXBar#(DirChildNum, L2Way));
    let m <- mkCRqXBar;
    return m;
endmodule
(* synthesize *)
module mkL2CRsToDirXBar(CRsXBar#(DirChildNum));
    let m <- mkCRsXBar;
    return m;
endmodule
(* synthesize *)
module mkDirPRqRsToL2XBar(PRqRsXBar#(DirChildNum, L2Way));
    let m <- mkPRqRsXBar;
    return m;
endmodule

// L1 + L2 + Dir
interface L1L2Dir;
    interface L1Proc#(L1Num, ProcRqId) to_proc;
    interface MemFifoClient#(DirCRqMshrIdx, void) to_mem;
endinterface

(* synthesize *)
module mkL1L2Dir(L1L2Dir) provisos(
    Add#(0, 0, LgL1BankNum),
    Add#(0, 0, LgL2BankNum),
    Add#(0, 0, LgDirBankNum),
    Mul#(L2Num, L2ChildNum, L1Num),
    Add#(L2Num, 0, DirChildNum)
);
    Vector#(L1Num, L1BankWrapper) l1 <- replicateM(mkL1BankWrapper);
    Vector#(L2Num, L2BankWrapper) l2 <- replicateM(mkL2BankWrapper);
    DirBankWrapper dir <- mkDirBankWrapper;

    // connect L1 <-> L2, loop for each L2
    for(Integer i = 0; i < valueOf(L2Num); i = i+1) begin
        // instantiate XBars
        let cRqL1XL2 <- mkL1CRqToL2XBar;
        let cRsL1XL2 <- mkL1CRsToL2XBar;
        let pL1XL2 <- mkL2PRqRsToL1XBar;
        // connect each L1 (which belongs to this L2) with XBar
        for(Integer j = 0; j < valueOf(L2ChildNum); j = j+1) begin
            L1BankWrapper ifcL1 = l1[i * valueOf(L2ChildNum) + j];
            mkConnection(cRqL1XL2.srcIfc[j], ifcL1.rqToPGet);
            mkConnection(cRsL1XL2.srcIfc[j], ifcL1.rsToPGet);
            mkConnection(pL1XL2.dstIfc[j], ifcL1.fromPPut);
        end
        // connect XBar with this L2
        mkConnection(cRqL1XL2.dstIfc[0], l2[i].rqFromCPut);
        mkConnection(cRsL1XL2.dstIfc[0], l2[i].rsFromCPut);
        mkConnection(pL1XL2.srcIfc[0], l2[i].toCGet);
    end

    // connect L2 <-> Dir
    // instantiate XBars
    let cRqL2XDir <- mkL2CRqToDirXBar;
    let cRsL2XDir <- mkL2CRsToDirXBar;
    let pL2XDir <- mkDirPRqRsToL2XBar;
    // connect each L1 (which belongs to this L2) with XBar
    for(Integer i = 0; i < valueOf(DirChildNum); i = i+1) begin
        mkConnection(cRqL2XDir.srcIfc[i], l2[i].rqToPGet);
        mkConnection(cRsL2XDir.srcIfc[i], l2[i].rsToPGet);
        mkConnection(pL2XDir.dstIfc[i], l2[i].fromPPut);
    end
    // connect XBar with this L2
    mkConnection(cRqL2XDir.dstIfc[0], dir.rqFromCPut);
    mkConnection(cRsL2XDir.dstIfc[0], dir.rsFromCPut);
    mkConnection(pL2XDir.srcIfc[0], dir.toCGet);

    function Server#(ProcRq#(ProcRqId, MemInst), ProcRs#(ProcRqId)) getL1Ifc(L1BankWrapper ifc);
        return (interface Server;
            interface request = ifc.rqFromCPut;
            interface response = ifc.toCGet;
        endinterface);
    endfunction
    interface to_proc = map(getL1Ifc, l1);
    interface to_mem = dir.to_mem;
endmodule
