import L1LLWrapper::*;
import HCC::*;
import HCCTypes::*;
import HCCWrapper::*;
import HCCTest::*;

////////// Connectal interfaces

interface HostIndication;
    method Action finish(Bit#(32) numResps);
endinterface

interface HostRequest;
    method Action start(Bit#(32) maxCycle);
endinterface

////////// Connectal interfaces end

interface Host;
    interface HostRequest request;
endinterface

module mkHost#(HostIndication indication) (Host);
    CCMem mem <- mkCCL1LL();
    // CCTest tester <- mkCCTestRandom(mem);
    CCTest tester <- mkCCTestShared(mem);
    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) ended <- mkReg(False);

    rule check_end (started && tester.isEnd && !ended);
        let n = tester.getThroughput;
        $display ("Trying to finish the test: #responses(%d)", n);
        indication.finish(n);
        ended <= True;
    endrule

    interface HostRequest request;
        method Action start(Bit#(32) maxCycle);
	    tester.start(maxCycle);
	    started <= True;
	endmethod
    endinterface
endmodule
