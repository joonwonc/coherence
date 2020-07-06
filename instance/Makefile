CONNECTALDIR?=../connectal
S2H_INTERFACES = HostRequest:Host.request
H2S_INTERFACES = Host:HostIndication

BSVFILES= Host.bsv
CPPFILES= SoftwareHost.cpp

HEMIOLA_LIB_PATH ?= ../../hemiola/syn/integration
PROC_LIB_PATH ?= ../../riscy-ooo/procs/lib

CONNECTALFLAGS += --mainclockperiod=40
CONNECTALFLAGS += --bscflags=" -aggressive-conditions +RTS -K100M -RTS -steps-max-intervals 10000000 "
# CONNECTALFLAGS += --bscflags=" -D USER_CLK_PERIOD=16 "
CONNECTALFLAGS += --bscflags=" -D RQ_TYPE_SEED=3 "
CONNECTALFLAGS += --bscflags=" -D RQ_BADDR_SEED=79 "
CONNECTALFLAGS += --bscflags=" -D CCSIZES_FILE=\\\"L1LLSimpleSizes.bsv\\\" "
CONNECTALFLAGS += --bsvpath="../src"
CONNECTALFLAGS += --bsvpath="../tests/include"
CONNECTALFLAGS += --bsvpath="$(HEMIOLA_LIB_PATH)"
CONNECTALFLAGS += --bsvpath="$(PROC_LIB_PATH)"

include $(CONNECTALDIR)/Makefile.connectal
