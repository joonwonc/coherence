
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
import CacheUtils::*;
import Types::*;
import CCTypes::*;
import CCSizes::*;
import SelfInvLLPipe::*;
import LLCRqMshr::*;
import SelfInvLLBank::*;

(* synthesize *)
module mkSelfInvLastLvCRqMshr(
    LLCRqMshr#(LLCRqNum, LLWay, LLTag, SelfInvDirPend#(LLChild), cRqT)
) provisos(
    Alias#(cRqT, LLRq#(LLCRqId, DmaRqId, LLChild))
);
    function Addr getAddr(cRqT r) = r.addr;
    let m <- mkLLCRqMshr(getAddr, getSelfInvNeedReqChild, getSelfInvDirPendInitVal);
    return m;
endmodule

(* synthesize *)
module mkSelfInvLLPipeline(
    mkSelfInvLLPipe#(LgLLBankNum, LLChildNum, LLWayNum, LLIndex, LLTag, LLCRqMshrIdx)
);
    let m <- mkLLPipe;
    return m;
endmodule

typedef SelfInvLLBank#(LgLLBankNum, LLChildNum, LLWayNum, LLIndexSz, LLTagSz, LLCRqNum, LLCRqId, DmaRqId) SelfInvLLBankWrapper;

(* synthesize *)
module mkSelfInvLLBankWrapper(LLBankWrapper);
    let m <- mkSelfInvLLBank(mkSelfInvLastLvCRqMshr, mkSelfInvLLPipeline);
    return m;
endmodule
