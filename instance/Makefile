TARGET ?= Top
BUILD_DIR ?= build
PROC_LIB_PATH ?= ../../riscy-ooo/procs/lib
BSV_MACROS ?= -D CCSIZES_FILE=\"L1LLSizes.bsv\"
BSC ?= bsc \
	-bdir $(BUILD_DIR) \
	-info-dir $(BUILD_DIR) \
	-simdir $(BUILD_DIR) \
	-vdir $(BUILD_DIR) \
	-aggressive-conditions \
	-p +:../src:$(PROC_LIB_PATH) \
	$(BSV_MACROS)

all: $(TARGET).bsv
	mkdir -p $(BUILD_DIR)
	$(BSC) -sim -u $(TARGET).bsv
	$(BSC) -sim -e mkTop -o $(TARGET)

clean:
	rm -rf $(BUILD_DIR)
