import L1LLWrapper::*;
import HCC::*;
import HCCTypes::*;
import HCCWrapper::*;
import HCCTest::*;

typedef `TEST_CYCLE_CNT TestCycleCnt;

(* synthesize *)
module mkTop(Empty);
    CC mem <- mkCCL1LL();

    CCTest tester <- mkCCTestCheck(mem);

    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) ended <- mkReg(False);

    rule start (!started);
        started <= True;
        tester.start(fromInteger(valueOf(TestCycleCnt)));
    endrule

    rule check_end (started && tester.isEnd && !ended);
        let n = tester.getThroughput();
        let m = tester.getMark();
        $display ("Test done, throughput: %d / %d", n, fromInteger(valueOf(TestCycleCnt)));
        $display ("Mark: %x", m);
        ended <= True;
    endrule

endmodule
