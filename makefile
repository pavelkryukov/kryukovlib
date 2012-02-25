HC := ghc
HC_FLAGS := -Wall -O3 -Werror -XMultiParamTypeClasses -XFlexibleContexts\
-XOverlappingInstances

BIN_DIR := bin
SRC_DIR := source
OBJ_DIR := obj
HI_DIR := hi

HC_FLAGS := $(HC_FLAGS) -i$(SRC_DIR) -hidir $(HI_DIR) -odir $(OBJ_DIR)

SRC_FILES := $(shell find $(SRC_DIR) -name "*.hs")

COURSE9_HS := $(shell ls $(SRC_DIR)/course9*.hs)
COURSE9_BIN := ${COURSE9_HS:$(SRC_DIR)/%.hs=$(BIN_DIR)/%}

RUNTEST_HS := $(SRC_DIR)/runtest.hs
RUNTEST_BIN := $(BIN_DIR)/runtest

ALL_BINS := $(COURSE9_BIN) $(RUNTEST_BIN)

$(BIN_DIR)/%: $(SRC_DIR)/%.hs $(BIN_DIR) $(SRC_FILES)
	$(HC) $(HC_FLAGS) $< -o $@
 
$(BIN_DIR):
	mkdir $(BIN_DIR)

all: $(ALL_BINS)

course9: $(COURSE9_BIN)

runtest: $(RUNTEST_BIN)

clean:
	rm -r $(OBJ_DIR)
	rm -r $(BIN_DIR)
	rm -r $(HI_DIR)
	