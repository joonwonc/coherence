
// Copyright (c) 2017 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Vector::*;
import RegFile::*;
import FIFO::*;
import FShow::*;
import GetPut::*;
import Types::*;
import CCTypes::*;
import DefaultValue::*;
import Ehr::*;
import MshrDeadlockChecker::*;

// MSHR dependency chain invariant:
// every cRq and pRq (for same addr) which has gone through pipeline once will be linked into the chain

// in L1, pRq is always directly handled at the end of pipeline

// PRq MSHR entry state
typedef enum {
    Empty,
    Init,
    Done
} L1PRqState deriving (Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState;
    L1PRqState state;
} L1PRqMshrStuck deriving(Bits, Eq, FShow);

interface L1PRqMshr_sendRsToP_pRq#(numeric type pRqNum);
    method PRqMsg#(void) getRq(Bit#(TLog#(pRqNum)) n);
    method Maybe#(Line) getData(Bit#(TLog#(pRqNum)) n);

    method Action releaseEntry(Bit#(TLog#(pRqNum)) n);
endinterface

interface L1PRqMshr_pipelineResp#(numeric type pRqNum);
    method PRqMsg#(void) getRq(Bit#(TLog#(pRqNum)) n);
    method L1PRqState getState(Bit#(TLog#(pRqNum)) n);

    method Action releaseEntry(Bit#(TLog#(pRqNum)) n);
    method Action setDone_setData(Bit#(TLog#(pRqNum)) n, Maybe#(Line) d);
endinterface

interface L1PRqMshr#(numeric type pRqNum);
    // port for pRqTransfer
    method ActionValue#(Bit#(TLog#(pRqNum))) getEmptyEntryInit(PRqMsg#(void) r);

    // port for sendRsToP_pRq
    interface L1PRqMshr_sendRsToP_pRq#(pRqNum) sendRsToP_pRq;

    // port for pipelineResp
    interface L1PRqMshr_pipelineResp#(pRqNum) pipelineResp;

    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(L1PRqMshrStuck) stuck;
endinterface


//////////////////
// safe version //
//////////////////
module mkL1PRqMshrSafe(
    L1PRqMshr#(pRqNum)
) provisos(
    Alias#(pRqIndexT, Bit#(TLog#(pRqNum)))
);
    // EHR port
    Integer sendRsToP_pRq_port = 0;
    Integer pipelineResp_port = 1;
    Integer pRqTransfer_port = 2;

    // MSHR entry state
    Vector#(pRqNum, Ehr#(3, L1PRqState)) stateVec <- replicateM(mkEhr(Empty));
    Vector#(pRqNum, Ehr#(3, PRqMsg#(void))) reqVec <- replicateM(mkEhr(?));
    // data valid bits
    Vector#(pRqNum, Ehr#(3, Bool)) dataValidVec <- replicateM(mkEhr(False));
    // data values
    RegFile#(pRqIndexT, Line) dataFile <- mkRegFile(0, fromInteger(valueOf(pRqNum) - 1));
    // empty entry FIFO
    FIFO#(pRqIndexT) emptyEntryQ <- mkSizedFIFO(valueOf(pRqNum));

    // empty entry FIFO needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(pRqIndexT) initIdx <- mkReg(0);

    // released entry index fifos
    FIFO#(pRqIndexT) releaseEntryQ_sendRsToP_pRq <- mkFIFO;
    FIFO#(pRqIndexT) releaseEntryQ_pipelineResp <- mkFIFO;

    rule initEmptyEntry(!inited);
        emptyEntryQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(pRqNum) - 1)) begin
            inited <= True;
            $display("%t L1PRqMshrSafe %m: init empty entry done", $time);
        end
    endrule

`ifdef CHECK_DEADLOCK
    MshrDeadlockChecker#(pRqNum) checker <- mkMshrDeadlockChecker;
    FIFO#(L1PRqMshrStuck) stuckQ <- mkFIFO1;

    (* fire_when_enabled *)
    rule checkDeadlock;
        let stuckIdx <- checker.getStuckIdx;
        if(stuckIdx matches tagged Valid .n) begin
            stuckQ.enq(L1PRqMshrStuck {
                addr: reqVec[n][0].addr,
                toState: reqVec[n][0].toState,
                state: stateVec[n][0]
            });
        end
    endrule
`endif

    rule doReleaseEntry_sendRsToP_pRq(inited);
        let n <- toGet(releaseEntryQ_sendRsToP_pRq).get;
        emptyEntryQ.enq(n);
`ifdef CHECK_DEADLOCK
        checker.releaseEntry(n);
`endif
    endrule

    (* descending_urgency = "doReleaseEntry_sendRsToP_pRq, doReleaseEntry_pipelineResp" *)
    rule doReleaseEntry_pipelineResp(inited);
        let n <- toGet(releaseEntryQ_pipelineResp).get;
        emptyEntryQ.enq(n);
`ifdef CHECK_DEADLOCK
        checker.releaseEntry(n);
`endif
    endrule

    method ActionValue#(pRqIndexT) getEmptyEntryInit(PRqMsg#(void) r) if(inited);
        emptyEntryQ.deq;
        pRqIndexT n = emptyEntryQ.first;
        stateVec[n][pRqTransfer_port] <= Init;
        reqVec[n][pRqTransfer_port] <= r;
        dataValidVec[n][pRqTransfer_port] <= False;
`ifdef CHECK_DEADLOCK
        checker.initEntry(n);
`endif
        return n;
    endmethod

    interface L1PRqMshr_sendRsToP_pRq sendRsToP_pRq;
        method PRqMsg#(void) getRq(pRqIndexT n);
            return reqVec[n][sendRsToP_pRq_port];
        endmethod

        method Maybe#(Line) getData(pRqIndexT n);
            return dataValidVec[n][sendRsToP_pRq_port] ? (Valid (dataFile.sub(n))) : Invalid;
        endmethod

        method Action releaseEntry(pRqIndexT n) if(inited);
            releaseEntryQ_sendRsToP_pRq.enq(n);
            stateVec[n][sendRsToP_pRq_port] <= Empty;
        endmethod
    endinterface

    interface L1PRqMshr_pipelineResp pipelineResp;
        method PRqMsg#(void) getRq(pRqIndexT n);
            return reqVec[n][pipelineResp_port];
        endmethod

        method L1PRqState getState(pRqIndexT n);
            return stateVec[n][pipelineResp_port];
        endmethod

        method Action setDone_setData(pRqIndexT n, Maybe#(Line) line);
            stateVec[n][pipelineResp_port] <= Done;
            dataValidVec[n][pipelineResp_port] <= isValid(line);
            dataFile.upd(n, fromMaybe(?, line));
        endmethod

        method Action releaseEntry(pRqIndexT n) if(inited);
            releaseEntryQ_pipelineResp.enq(n);
            stateVec[n][pipelineResp_port] <= Empty;
        endmethod
    endinterface

`ifdef CHECK_DEADLOCK
    interface stuck = toGet(stuckQ);
`else
    interface stuck = nullGet;
`endif
endmodule

// exportd version
module mkL1PRqMshr(L1PRqMshr#(pRqNum));
    let m <- mkL1PRqMshrSafe;
    return m;
endmodule
