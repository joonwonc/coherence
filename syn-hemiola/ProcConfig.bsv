// just to make things compile
`define rv64 True
`define m True
`define a True
`define f True
`define d True
`define sizeSup 1
`define NUM_CORES 1
`define NUM_EPOCHS 1
`define NUM_SPEC_TAGS 1
`define ROB_SIZE 1
`define LDQ_SIZE 1
`define STQ_SIZE 1
`define SB_SIZE 1
`define DRAM_MAX_REQS 1
`define DRAM_MAX_READS 1
`define DRAM_MAX_WRITES 1
`define DRAM_LATENCY 1
`define LOG_BOOT_ROM_BYTES 12

`define LOG_LLC_LINES 1
`define LOG_LLC_WAYS 1
`define DTLB_REQ_NUM 1
`define L2TLB_REQ_NUM 1
`define BOOKKEEPING_FP_FMA_SIZE 1
`define LOG_L2_TLB_4KB_WAYS 1
`define LOG_L2_TLB_4KB_SIZE 1
`define LOG_L1_WAYS 1
`define LOG_L1_LINES 1
`define L1_TLB_SIZE 1
`define BOOKKEEPING_INT_MUL_SIZE 1
`define L2_TLB_HUGE_SIZE 1
`define USER_CLK_PERIOD 16

// used for checking deadlock
`define LOG_DEADLOCK_CYCLES 26
