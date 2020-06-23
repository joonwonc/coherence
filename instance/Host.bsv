import L1LLSimple::*;

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
    L1LLSimple mem <- mkL1LLI();
    // CCTest tester <- mkCCTestRandom(mem);
    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) ended <- mkReg(False);

    rule check_end (started && !ended);
        // let n = tester.getThroughput;
        // indication.finish(n);
        indication.finish(0);
        ended <= True;
    endrule

    interface HostRequest request;
        method Action start(Bit#(32) maxCycle);
	    // tester.start(maxCycle);
	    started <= True;
	endmethod
    endinterface
endmodule
