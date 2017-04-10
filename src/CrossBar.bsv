import Vector::*;
import GetPut::*;
import CacheUtils::*;
import CCTypes::*;
import Types::*;
import FShow::*;
import Fifo::*;
import Ehr::*;

typedef struct {
    dstIdxT idx;
    dstDataT data;
} XBarDstInfo#(type dstIdxT, type dstDataT) deriving(Bits, Eq);

module mkXBar#(
    function XBarDstInfo#(dstIdxT, dstDataT) getDstInfo(srcIdxT idx, srcDataT data),
    Vector#(srcNum, Get#(srcDataT)) srcIfc,
    Vector#(dstNum, Put#(dstDataT)) dstIfc
)(Empty) provisos(
    Alias#(srcIdxT, Bit#(TLog#(srcNum))),
    Alias#(dstIdxT, Bit#(TLog#(dstNum))),
    Bits#(srcDataT, _srcDataSz),
    Bits#(dstDataT, _dstDataSz),
    FShow#(dstDataT)
);
    // proposed data transfer by each src
    Vector#(srcNum, Ehr#(2, Maybe#(dstIdxT))) propDstIdx <- replicateM(mkEhr(Invalid));
    Vector#(srcNum, Ehr#(2, dstDataT)) propDstData <- replicateM(mkEhr(?));
    // enq command that should be carried out
    Vector#(dstNum, Ehr#(2, Maybe#(dstDataT))) enqDst <- replicateM(mkEhr(Invalid));

    // src propose data transfer when src is not empty 
    // and there is no unfinished src proposal
    for(Integer i = 0; i < valueOf(srcNum); i = i+1) begin
        rule srcPropose(!isValid(propDstIdx[i][0]));
            let d <- srcIfc[i].get;
            let info = getDstInfo(fromInteger(i), d);
            propDstIdx[i][0] <= Valid (info.idx);
            propDstData[i][0] <= info.data;
        endrule
    end

    // round-robin select src for each dst
    Vector#(dstNum, Reg#(srcIdxT)) srcRR <- replicateM(mkReg(0));

    // do selection for each dst & generate enq commands & deq src proposals
    // dst which has unfinished enq command cannot select src
    (* fire_when_enabled, no_implicit_conditions *)
    rule dstSelectSrc;
        // which src to deq (i.e. select) by each dst
        Vector#(dstNum, Maybe#(srcIdxT)) deqSrcByDst = replicate(Invalid);
        // each dst selects
        for(Integer dst = 0; dst < valueOf(dstNum); dst = dst + 1) begin
            // only select src to deq when dst has no unfinished enq command
            if(!isValid(enqDst[dst][0])) begin
                function Bool isFromSrc(srcIdxT src);
                    // src has proposed data to this dst or not
                    if(propDstIdx[src][1] matches tagged Valid .dstIdx &&& dstIdx == fromInteger(dst)) begin
                        return True;
                    end
                    else begin
                        return False;
                    end
                endfunction
                Maybe#(srcIdxT) whichSrc = Invalid; // the src to select
                if(isFromSrc(srcRR[dst])) begin
                    // first check the src with priority
                    whichSrc = Valid (srcRR[dst]);
                end
                else begin
                    // otherwise just get one valid src
                    Vector#(srcNum, srcIdxT) srcIdxVec = genWith(fromInteger);
                    whichSrc = searchIndex(isFromSrc, srcIdxVec);
                end
                if(whichSrc matches tagged Valid .src) begin
                    // can do enq & deq
                    deqSrcByDst[dst] = whichSrc;
                    enqDst[dst][0] <= Valid (propDstData[src][1]); // set enq command
                    // change round robin
                    srcRR[dst] <= srcRR[dst] == fromInteger(valueOf(srcNum) - 1) ? 0 : srcRR[dst] + 1;
                end
            end
        end

        // deq selected src
        function Bool isDeqSrc(srcIdxT src);
            function Bool isMatch(Maybe#(srcIdxT) deqIdx);
                return deqIdx matches tagged Valid .idx &&& idx == src ? True : False;
            endfunction
            return any(isMatch, deqSrcByDst);
        endfunction
        for(Integer i = 0; i < valueOf(srcNum); i = i+1) begin
            if(isDeqSrc(fromInteger(i))) begin
                propDstIdx[i][1] <= Invalid;
                $display("%t XBar %m: deq src %d", $time, i);
                doAssert(isValid(propDstIdx[i][1]), "src must be proposing");
            end
        end
    endrule

    // enq dst & change round robin
    for(Integer i = 0; i < valueOf(dstNum); i = i+1) begin
        // XXX since dstIfc.put may conflict with other rules
        // the following rule should only fire when it can make progress
        // otherwise it may prevent other rules from firing forever
        rule doEnq(enqDst[i][1] matches tagged Valid .d);
            dstIfc[i].put(d);
            enqDst[i][1] <= Invalid; // reset enq command
            $display("%t XBAR %m: enq dst %d ; ", $time, i, fshow(d));
        endrule
    end
endmodule

interface CrossBar#(
    numeric type srcNum,
    type srcDataT,
    numeric type dstNum,
    type dstDataT
);
    interface Vector#(srcNum, FifoEnq#(srcDataT)) srcIfc;
    interface Vector#(dstNum, FifoDeq#(dstDataT)) dstIfc;
endinterface

module mkCrossBar#(
    function XBarDstInfo#(dstIdxT, dstDataT) getDstInfo(srcIdxT idx, srcDataT data)
)(
    CrossBar#(srcNum, srcDataT, dstNum, dstDataT)
) provisos(
    Alias#(srcIdxT, Bit#(TLog#(srcNum))),
    Alias#(dstIdxT, Bit#(TLog#(dstNum))),
    Bits#(srcDataT, _srcDataSz),
    Bits#(dstDataT, _dstDataSz),
    FShow#(dstDataT)
);

    // in/out FIFOs
    Vector#(srcNum, Fifo#(2, srcDataT)) srcQ <- replicateM(mkCFFifo);
    Vector#(dstNum, Fifo#(2, dstDataT)) dstQ <- replicateM(mkCFFifo);

    mkXBar(getDstInfo, map(toGet, srcQ), map(toPut, dstQ));

    interface srcIfc = map(toFifoEnq, srcQ);
    interface dstIfc = map(toFifoDeq, dstQ);
endmodule

/*
module mkCrossBar#(
    function XBarDstInfo#(dstIdxT, dstDataT) getDstInfo(srcIdxT idx, srcDataT data)
)(
    CrossBar#(srcNum, srcDataT, dstNum, dstDataT)
) provisos(
    Alias#(srcIdxT, Bit#(TLog#(srcNum))),
    Alias#(dstIdxT, Bit#(TLog#(dstNum))),
    Bits#(srcDataT, _srcDataSz),
    Bits#(dstDataT, _dstDataSz),
    FShow#(dstDataT)
);

    // in/out FIFOs
    Vector#(srcNum, Fifo#(2, srcDataT)) srcQ <- replicateM(mkCFFifo);
    Vector#(dstNum, Fifo#(2, dstDataT)) dstQ <- replicateM(mkCFFifo);

    // deq & enq command that should be carried out
    Vector#(srcNum, Ehr#(2, Bool)) deqSrc <- replicateM(mkEhr(False));
    Vector#(dstNum, Ehr#(2, Maybe#(dstDataT))) enqDst <- replicateM(mkEhr(Invalid));

    // proposed data transfer by each src
    Vector#(srcNum, Ehr#(2, Maybe#(dstIdxT))) propDstIdx <- replicateM(mkEhr(Invalid));
    Vector#(srcNum, Ehr#(2, dstDataT)) propDstData <- replicateM(mkEhr(?));

    // src propose data transfer when src is not empty 
    // and there is no unfinished deq command for this src
    for(Integer i = 0; i < valueOf(srcNum); i = i+1) begin
        (* fire_when_enabled *)
        rule srcPropose;
            Maybe#(dstIdxT) propDst = Invalid;
            dstDataT propData = ?;
            if(srcQ[i].notEmpty && !deqSrc[i][0]) begin
                let info = getDstInfo(fromInteger(i), srcQ[i].first);
                propDst = Valid (info.idx);
                propData = info.data;
            end
            propDstIdx[i][0] <= propDst;
            propDstData[i][0] <= propData;
        endrule
    end

    // reset propose ehr at the end of every cycle
    (* fire_when_enabled, no_implicit_conditions *)
    rule resetPropose;
        for(Integer i = 0; i < valueOf(srcNum); i = i+1) begin
            propDstIdx[i][1] <= Invalid;
        end
    endrule

    // round-robin select src for each dst
    Vector#(dstNum, Reg#(srcIdxT)) srcRR <- replicateM(mkReg(0));

    // do selection for each dst & generate enq deq commands
    // dst which has full fifo or unfinished enq command cannot select src
    (* fire_when_enabled, no_implicit_conditions *)
    rule dstSelectSrc;
        // which src to deq by this dst
        Vector#(dstNum, Maybe#(srcIdxT)) deqSrcByDst = replicate(Invalid);
        // each dst selects
        for(Integer dst = 0; dst < valueOf(dstNum); dst = dst + 1) begin
            // only select src to deq when dst can be enq & no unfinished enq command
            if(dstQ[dst].notFull && !isValid(enqDst[dst][0])) begin
                function Bool isFromSrc(srcIdxT src);
                    // srcQ[src] has proposed data to this dst or not
                    if(propDstIdx[src][1] matches tagged Valid .dstIdx &&& dstIdx == fromInteger(dst)) begin
                        return True;
                    end
                    else begin
                        return False;
                    end
                endfunction
                Maybe#(srcIdxT) whichSrc = Invalid; // the src to select
                if(isFromSrc(srcRR[dst])) begin
                    // first check the src with priority
                    whichSrc = Valid (srcRR[dst]);
                end
                else begin 
                    // otherwise just get one valid src
                    Vector#(srcNum, srcIdxT) srcIdxVec = genWith(fromInteger);
                    whichSrc = searchIndex(isFromSrc, srcIdxVec);
                end
                if(whichSrc matches tagged Valid .src) begin
                    // can do enq & deq
                    deqSrcByDst[dst] = whichSrc;
                    enqDst[dst][0] <= Valid (propDstData[src][1]); // set enq command
                end
            end
        end

        // deq signal for each src
        function Bool isDeqSrc(srcIdxT src);
            function Bool isMatch(Maybe#(srcIdxT) deqIdx);
                return deqIdx matches tagged Valid .idx &&& idx == src ? True : False;
            endfunction
            return any(isMatch, deqSrcByDst);
        endfunction
        for(Integer i = 0; i < valueOf(srcNum); i = i+1) begin
            Bool deq = isDeqSrc(fromInteger(i));
            // only set deq command when there is no unfinished one
            // otherwise we may overwrite a deq command
            if(!deqSrc[i][0]) begin
                deqSrc[i][0] <= deq; // set deq command
            end
            else begin
                doAssert(!deq, "cannot double-deq src");
            end
        end
    endrule

    // deq src
    for(Integer i = 0; i < valueOf(srcNum); i = i+1) begin
        (* fire_when_enabled *)
        rule doDeq;
            if(deqSrc[i][1]) begin
                srcQ[i].deq;
                $display("%t XBar %m con: deq src %d", $time, i);
                deqSrc[i][1] <= False; // reset deq command
            end
        endrule
    end

    // enq dst & change round robin
    for(Integer i = 0; i < valueOf(dstNum); i = i+1) begin
        (* fire_when_enabled *)
        rule doEnq;
            if(enqDst[i][1] matches tagged Valid .d) begin
                dstQ[i].enq(d);
                enqDst[i][1] <= Invalid; // reset enq command
                srcRR[i] <= srcRR[i] == fromInteger(valueOf(srcNum) - 1) ? 0 : srcRR[i] + 1;
                $display("%t XBAR %m con: enq dst %d ; ", $time, i, fshow(d));
            end
        endrule
    end

    interface srcIfc = map(toFifoEnq, srcQ);
    interface dstIfc = map(toFifoDeq, dstQ);
endmodule
*/
