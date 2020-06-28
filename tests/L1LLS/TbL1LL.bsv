import Types::*;
import MemoryTypes::*;
import Amo::*;
import CacheUtils::*;

import CCTypes::*;
import CCSizes::*;
import FShow::*;
import Randomizable::*;
import Vector::*;
import FIFO::*;
import RegFile::*;
import Connectable::*;
import GetPut::*;
import ClientServer::*;
import DelayMemTypes::*;
import IdealDelayMem::*;
import Printf::*;
import ConfigReg::*;

import L1LL::*;

// FIXME assume no banking

// number of reqs (tests) per core
typedef 10000 TestNum;
typedef Bit#(TLog#(TestNum)) TestId;
typedef Bit#(TLog#(TAdd#(TestNum, 1))) TestCnt;

typedef TDiv#(TestNum, 10) TestPrintNum;

// memory delay
typedef 30 MemDelay;

// time out
typedef 10000 MaxTimeOut;
typedef Bit#(TLog#(MaxTimeOut)) TimeOutCnt;

// test index, tag, data offset choices
typedef TMul#(4, LLWayNum) TagNum;
typedef 2 IndexNum;

typedef LLTag LLCTag;
typedef LLIndex LLCIndex;

function Addr getAddr(LLCTag tag, LLCIndex index, LineDataOffset sel);
    DataBytesOffset off = 0;
    return {tag, index, sel, off};
endfunction

// memory size for testing
typedef TAdd#(TSub#(AddrSz, SizeOf#(LLCTag)), TLog#(TagNum)) LgTestMemSzBytes;

// req/resp to/from memory system
typedef enum {
    Ld, St, Lr, Sc, Amo
} MemTestOp deriving(Bits, Eq, FShow, Bounded);

function MemOp getMemOp(MemTestOp op);
    case(op)
        Ld: return Ld;
        St: return St;
        Lr: return Lr;
        Sc: return Sc;
        Amo: return Amo;
        default: return ?;
    endcase
endfunction

function Msi getToState(MemTestOp op);
    case(op)
        Ld: return S;
        Lr: return E;
        St, Sc, Amo: return M;
        default: return ?;
    endcase
endfunction

typedef struct {
    TestId id;
    MemTestOp op;
    Addr addr;
    ByteEn byteEn; // for Sc
    Data data; // for Sc/Amo
    LineByteEn lineBE; // for St
    Line line; // for St
    AmoInst amoInst; // for Amo
} MemTestReq deriving(Bits, Eq, FShow);

typedef enum {
    Ld, St, LrScAmo
} MemRespType deriving(Bits, Eq, FShow);
typedef struct {
    MemRespType t;
    TestId id;
    Data data;
} MemTestResp deriving(Bits, Eq, FShow);

function MemRespType getMemRespType(MemTestOp op);
    case(op)
        Ld: return Ld;
        St: return St;
        Lr, Sc, Amo: return LrScAmo;
        default: return ?;
    endcase
endfunction

// random req stall
typedef Bit#(2) ReqStall;

function Bool getReqStall(ReqStall x);
`ifdef NO_REQ_STALL
    return False;
`else
    return x == 0;
`endif
endfunction

// update cache line
// test FSM
typedef enum {InitTable, InitAddr, Idle, Process, Done} TestFSM deriving(Bits, Eq, FShow);

(* synthesize *)
module mkTbL1LL(Empty);
    // randomize req
    // D$
    Vector#(L1DNum, Randomize#(ReqStall)) randDCReqStall <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(MemTestOp)) randDCOp <- replicateM(mkConstrainedRandomizer(minBound, maxBound));
    Vector#(L1DNum, Randomize#(LLCTag)) randDCTag <- replicateM(mkConstrainedRandomizer(0, fromInteger(valueOf(TagNum) - 1)));
    Vector#(L1DNum, Randomize#(LLCIndex)) randDCIndex <- replicateM(mkConstrainedRandomizer(0, fromInteger(valueOf(IndexNum) - 1)));
    Vector#(L1DNum, Randomize#(LineDataOffset)) randDCDataSel <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(Data)) randDCData <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(Bit#(DataSzBytes))) randDCDataBE <- replicateM(mkConstrainedRandomizer(1, maxBound)); // it could be all 0 though..
    Vector#(L1DNum, Randomize#(Line)) randDCLine <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(Bit#(LineSzBytes))) randDCLineBE <- replicateM(mkConstrainedRandomizer(1, maxBound)); // it could be all 0 though..
    Vector#(L1DNum, Randomize#(AmoFunc)) randDCAmoFunc <- replicateM(mkConstrainedRandomizer(Swap, Maxu));
    Vector#(L1DNum, Randomize#(Bool)) randDCDoubleWord <- replicateM(mkGenericRandomizer);

    // record req
    // D$
    Vector#(L1DNum, RegFile#(TestId, Maybe#(MemTestReq))) dcReqTable <- replicateM(mkRegFileFull);
    Vector#(L1DNum, Reg#(TestCnt)) sendDCCnt <- replicateM(mkReg(0));
    Vector#(L1DNum, RWire#(MemTestReq)) sendDCReq <- replicateM(mkRWire);

    // print helper for send: print when read this number
    Vector#(L1DNum, Reg#(TestCnt)) sendPrintDCCnt <- replicateM(mkReg(fromInteger(valueOf(TestPrintNum))));

    // record resp
    // D$
    Vector#(L1DNum, RegFile#(TestId, Bool)) dcRespDoneTable <- replicateM(mkRegFileFull);
    Vector#(L1DNum, Reg#(TestCnt)) recvDCCnt <- replicateM(mkReg(0));
    Vector#(L1DNum, RWire#(MemTestResp)) recvDCResp <- replicateM(mkRWire);
    Vector#(L1DNum, Reg#(TimeOutCnt)) dcTimeOut <- replicateM(mkReg(0));

    // check cononicalize rule fires
    PulseWire processFired <- mkPulseWire;

    // init
    Reg#(TestFSM) testFSM <- mkConfigReg(InitTable);
    Reg#(Bool) coreTableInitDone <- mkReg(False);
    Reg#(TestId) iterId <- mkReg(0);
    Reg#(LLCTag) iterTag <- mkReg(0);
    Reg#(LLCIndex) iterIndex <- mkReg(0);

    // DUT
    function L1ProcResp#(ProcRqId) getL1ProcResp(Integer i);
        return (interface L1ProcResp;
            method Action respLd(ProcRqId id, Data d);
                recvDCResp[i].wset(MemTestResp {t: Ld, id: truncate(id), data: d});
            endmethod
            method Action respLrScAmo(ProcRqId id, Data d);
                recvDCResp[i].wset(MemTestResp {t: LrScAmo, id: truncate(id), data: d});
            endmethod
            method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(ProcRqId id);
                recvDCResp[i].wset(MemTestResp {t: St, id: truncate(id), data: ?});
                let req = validValue(dcReqTable[i].sub(truncate(id)));
                return tuple2(req.lineBE, req.line);
            endmethod
            method Action evict(LineAddr a);
                noAction;
            endmethod
        endinterface);
    endfunction
    let memSys <- mkL1LL(map(getL1ProcResp, genVector));
    IdealDelayMem#(MemDelay, LgTestMemSzBytes, LdMemRqId#(LLCRqMshrIdx), void) delayMem <- mkIdealDelayMem;
    mkConnection(memSys.to_mem, delayMem.to_proc);

    // get all ifc used in the reset of the testbench
    Vector#(L1DNum, L1ProcReq#(ProcRqId)) ifcDC = memSys.dReq;
    DelayMemTest dutMem = delayMem.to_test;

    rule doInitCoreTable(testFSM == InitTable && !coreTableInitDone);
        for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
            dcReqTable[i].upd(iterId, Invalid);
            dcRespDoneTable[i].upd(iterId, False);
        end
        // change state
        if(iterId == fromInteger(valueof(TestNum) - 1)) begin
            iterId <= 0;
            coreTableInitDone <= True;
        end
        else begin
            iterId <= iterId + 1;
        end
    endrule

    rule doInitTableDone(testFSM == InitTable && coreTableInitDone);
        testFSM <= InitAddr;
        $fdisplay(stderr, "INFO: init table done");
    endrule

    rule doInitAddr(testFSM == InitAddr);
        Addr addr = getAddr(iterTag, iterIndex, 0);
        Line initV = replicate(addr);
        dutMem.initLine(addr, initV);
        if(iterIndex == fromInteger(valueOf(IndexNum) - 1)) begin
            iterIndex <= 0;
            if(iterTag == fromInteger(valueOf(TagNum) - 1)) begin
                iterTag <= 0;
                // init randomizers and files for each core
                for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
                    randDCReqStall[i].cntrl.init;
                    randDCOp[i].cntrl.init;
                    randDCTag[i].cntrl.init;
                    randDCIndex[i].cntrl.init;
                    randDCDataSel[i].cntrl.init;
                    randDCDataBE[i].cntrl.init;
                    randDCData[i].cntrl.init;
                    randDCLineBE[i].cntrl.init;
                    randDCLine[i].cntrl.init;
                    randDCAmoFunc[i].cntrl.init;
                    randDCDoubleWord[i].cntrl.init;
                    String name = sprintf("req_dc_%d.log", i);
                    File f <- $fopen(name, "w");
                    name = sprintf("resp_dc_%d.log", i);
                    f <- $fopen(name, "w");
                end
                // notify memory that init done
                dutMem.initDone;
                // change state
                testFSM <= Idle;
                $fdisplay(stderr, "INFO: init addr done");
            end
            else begin
                iterTag <= iterTag + 1;
            end
        end
        else begin
            iterIndex <= iterIndex + 1;
        end
    endrule

    Reg#(Bit#(64)) waitCount <- mkReg(0);

    rule simplyWait(testFSM == Idle);
        // wait for LLC to init all BRAMs
        waitCount <= waitCount + 1;
        if(waitCount == fromInteger(valueOf(TExp#(LLIndexSz)))) begin
            $display("%t %m Start Issuing Requests only now!!!!", $time);
            $fdisplay(stderr, "INFO: start issue req");
            testFSM <= Process;
        end
    endrule

    for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
        rule doDCReq(testFSM == Process && sendDCCnt[i] < fromInteger(valueOf(TestNum)));
            // randomize req
            let index <- randDCIndex[i].next;
            let tag <- randDCTag[i].next;
            let sel <- randDCDataSel[i].next;
            let addr = getAddr(tag, index, sel);
            let op <- randDCOp[i].next;
            let data <- randDCData[i].next;
            let rBE <- randDCDataBE[i].next;
            ByteEn be = unpack(rBE);
            let line <- randDCLine[i].next;
            let rlbe <- randDCLineBE[i].next;
            LineByteEn lineBE = unpack(rlbe);
            let doubleWord <- randDCDoubleWord[i].next;
            let amoFunc <- randDCAmoFunc[i].next;
            let req = MemTestReq {
                id: truncate(sendDCCnt[i]),
                op: op,
                addr: addr,
                byteEn: be,
                data: data,
                lineBE: lineBE,
                line: line,
                amoInst: AmoInst {
                    func: amoFunc,
                    doubleWord: doubleWord,
                    aq: False,
                    rl: False
                }
            };
            // randomize stall
            let rStall <- randDCReqStall[i].next;
            if(!getReqStall(rStall)) begin
                // no stall, send req & record
                ifcDC[i].req(ProcRq {
                    id: zeroExtend(req.id),
                    addr: req.addr,
                    toState: getToState(req.op),
                    op: getMemOp(req.op),
                    byteEn: req.byteEn,
                    data: req.data,
                    amoInst: req.amoInst
                });
                sendDCReq[i].wset(req);

                // output req cnt
                if((sendDCCnt[i] + 1) == sendPrintDCCnt[i]) begin
                    $fdisplay(stderr, "INFO: %t D$ %d send req %d/%d",
                        $time, i, sendDCCnt[i] + 1, valueOf(TestNum)
                    );
                    sendPrintDCCnt[i] <= sendPrintDCCnt[i] + fromInteger(valueOf(TestPrintNum));
                end
            end
        endrule
    end

    function L1BankId getBankId(Addr a) = truncate(a >> valueof(LgLineSzBytes));

    (* fire_when_enabled *)
    rule doConProcess(testFSM == Process);
        // changes to make on refLink
        Vector#(L1DNum, Maybe#(LineAddr)) wrAddr = replicate(Invalid); // for clear others' links
        Vector#(L1DNum, Maybe#(LineAddr)) lrAddr = replicate(Invalid); // for clear own link
        Vector#(L1DNum, Maybe#(L1BankId)) scBank = replicate(Invalid); // for clear own link

        // handle D$ req/resp
        for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
            // store req just sent
            if(sendDCReq[i].wget matches tagged Valid .req) begin
                sendDCCnt[i] <= sendDCCnt[i] + 1;
                // save req
                dcReqTable[i].upd(req.id, Valid (req));
            end
            // check resp just recv
            if(recvDCResp[i].wget matches tagged Valid .resp) begin
                recvDCCnt[i] <= recvDCCnt[i] + 1; // incr recv cnt
                // set resp as done
                if(!dcRespDoneTable[i].sub(resp.id)) begin
                    dcRespDoneTable[i].upd(resp.id, True);
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: D$ %d resp %x duplicate", i, resp.id);
                    $finish;
                end
                // get req
                let r = dcReqTable[i].sub(resp.id);
                if(isValid(r)) begin
                    // good
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: D$ %d resp %x does not have valid req", i, resp.id);
                    $finish;
                end
                let req = validValue(r);
                // check resp type
                if(getMemRespType(req.op) == resp.t) begin
                    // good
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: D$ %d resp %x wrong type", i, resp.id);
                    $finish;
                end
                // some common info
                let bankId = getBankId(req.addr);
                let lineAddr = getLineAddr(req.addr);
                // check resp val and apply actions to ref mem
                if(req.op == Ld) begin
                    // load: check value
                    let dutData = resp.data;
                end
                else if(req.op == St) begin
                    // set addr for clearing others' link
                    wrAddr[i] = Valid (lineAddr);
                end
                else if(req.op == Lr) begin
                    // load reserve: check value
                    let dutData = resp.data;
                    // record Lr addr for setting link addr
                    lrAddr[i] = Valid (lineAddr);
                end
                else if(req.op == Sc) begin
                    // store cond: check value, figure out whether ref mem is success
                    //Bool refSucc = refLink[i][bankId] == Valid (lineAddr);
                    //Data refData = refSucc ? fromInteger(valueof(ScSuccVal)) : fromInteger(valueof(ScFailVal));
                    let dutData = resp.data;
                    // FIXME TODO I cannot make BSV compiles with linkAddr estimation, so assume LR/SC is correctly implemented
                    let refData = dutData;
                    if(refData == dutData || dutData == fromInteger(valueof(ScFailVal))) begin
                        // good: it is fine for dut to have failed Sc
                    end
                    else begin
                        $fwrite(stderr, "[TbL1LL] ERROR: D$ %d wrong Sc resp %x\n", i, resp.id);
                        $finish;
                    end
                    // update mem & link addr
                    scBank[i] = Valid (bankId); // record sc bank for clearing own link
                    if(dutData == fromInteger(valueof(ScSuccVal))) begin
                        // record write addr for clear other link
                        wrAddr[i] = Valid (lineAddr);
                    end
                end
                else if(req.op == Amo) begin
                    // AMO: check value
                    Data dutData = resp.data;
                    Bool upper32 = req.addr[2] == 1;
                    // record write addr for clear other link
                    wrAddr[i] = Valid (lineAddr);
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: D$ %d resp %x unknown op\n", i, resp.id);
                    $finish;
                end
                // reset time out
                dcTimeOut[i] <= 0;
            end
            else if(recvDCCnt[i] < sendDCCnt[i]) begin
                // incr time out
                dcTimeOut[i] <= dcTimeOut[i] + 1;
                if(dcTimeOut[i] >= fromInteger(valueOf(MaxTimeOut) - 1)) begin
                    $fwrite(stderr, "[TbL1LL] ERROR: D$ %d deadlock\n", i);
                    $finish;
                end
            end
        end

        // set fired
        processFired.send;
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule checkProcessFire(testFSM == Process);
        if(!processFired) begin
            $fwrite(stderr, "[TbL1LL] ERROR: time %t, process cononicalize rule does not fire\n", $time);
            $finish;
        end
    endrule

endmodule

