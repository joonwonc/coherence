import L1LLWrapper::*;
import HCC::*;
import HCCTypes::*;
import HCCWrapper::*;
import HCCTest::*;

typedef `TEST_CYCLE_CNT TestCycleCnt;

(* synthesize *)
module mkTop(Empty);
    CCMem mem <- mkCCL1LL();

    // CCTest tester <- mkCCTestIsolated(mem.cc);
    CCTest tester <- mkCCTestShared(mem.cc);
    // CCTest tester <- mkCCTestRandom(mem.cc);
    // CCTest tester <- mkCCTestCheck(mem.cc);
    // CCTest tester <- mkCCTestCheckIdxEquiv(mem.cc);

    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) ended <- mkReg(False);

    rule start (!started);
        started <= True;
        tester.start(fromInteger(valueOf(TestCycleCnt)));
    endrule

    rule check_end (started && tester.isEnd && !ended);
        let n = tester.getThroughput();
        let m = tester.getMark();
        $fwrite (stderr, "Test done, throughput: %d / %d", n, fromInteger(valueOf(TestCycleCnt)));
        $fwrite (stderr, "Mark: %x", m);
        ended <= True;
    endrule

endmodule
