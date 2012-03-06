HC := ghc
HC_FLAGS := -Wall -O3 -Werror -XMultiParamTypeClasses -XFlexibleContexts\
-XOverlappingInstances

BIN_DIR := bin
SRC_DIR := source
OBJ_DIR := obj
HI_DIR := hi

HC_FLAGS := $(HC_FLAGS) -i$(SRC_DIR) -hidir $(HI_DIR) -odir $(OBJ_DIR)

COURSE9_BIN := $(BIN_DIR)/course9
COURSE9_ANALYTIC_BIN := $(BIN_DIR)/course9-analytic
RUNTEST_BIN := $(BIN_DIR)/runtest

$(BIN_DIR)/%: $(SRC_DIR)/%.hs $(BIN_DIR)
	$(HC) $(HC_FLAGS) $< -o $@
 
$(BIN_DIR):
	mkdir $(BIN_DIR)

course9: $(COURSE9_BIN)
course9-analytic: $(COURSE9_ANALYTIC_BIN)
runtest: $(RUNTEST_BIN)

clean:
	rm -r $(OBJ_DIR)
	rm -r $(BIN_DIR)
	rm -r $(HI_DIR)
	