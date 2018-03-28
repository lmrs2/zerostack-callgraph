include ../../common.mk

CHECKER_SOURCES := FPness.cpp
CHECKER_TARGET := FPness.$(LIBEXT)

PASS_SOURCES := FunctionPointerAnno.cpp ../../AnnotationInfo.cpp

PASS_TARGET := FunctionPointerAnno.$(LIBEXT)

CHECKER_OBJS := $(CHECKER_SOURCES:%.cpp=%.o)
PASS_OBJS := $(PASS_SOURCES:%.cpp=%.o)

CXXFLAGS += -I../.. -I$(LLVM_SRC_DIR)/tools/clang/include/ -I$(LLVM_BUILD_DIR)/tools/clang/include/

USER := $(if $(SUDO_USER),$(SUDO_USER),$(whoami))

.PHONY: all
all: $(CHECKER_TARGET) $(PASS_TARGET)
# install

# Build the Clang plugin module.
$(CHECKER_TARGET): $(CHECKER_OBJS)
	$(CXX) $(PLUGIN_LDFLAGS) $(CXXFLAGS) \
		$(LLVM_CXXFLAGS) $(LLVM_LDFLAGS) \
		-o $@ $^

# Build the LLVM pass module. -I../../../tools/clang/include/ -I../../../include/ -I../../../build/include/ -std=c++11
$(PASS_TARGET): $(PASS_OBJS)
	$(CXX) $(PLUGIN_LDFLAGS) $(CXXFLAGS) \
		$(LLVM_CXXFLAGS) $(LLVM_LDFLAGS) \
		-o $@ $^

# $(LLVM_CONFIG) $(LLVM_BUILD)
%.o: %.cpp
	$(CXX) -c $(CXXFLAGS) $(LLVM_CXXFLAGS) \
		-o $@ $<

.PHONY: clean
clean:
	rm -rf $(CHECKER_TARGET) $(CHECKER_OBJS) $(PASS_TARGET) $(PASS_OBJS)

install: FPness.$(LIBEXT) FunctionPointerAnno.$(LIBEXT)
	@cp FPness.$(LIBEXT) $(LLVM_LIB_DIR) 
	@cp FunctionPointerAnno.$(LIBEXT) $(LLVM_LIB_DIR)
	@cp fpointer-cc $(LLVM_BIN_DIR)/CG-clang	&& chmod a+rx $(LLVM_BIN_DIR)/CG-clang
	@cp cg-compiler $(LLVM_BIN_DIR)/cg-compiler && chmod a+rx $(LLVM_BIN_DIR)/cg-compiler
	@touch /tmp/metafile_pass && chown $(USER) /tmp/metafile_pass	
# Testing stuff.
.PHONY: test smoke
#test: $(TARGET)
#	$(BUILD)/llvm/bin/llvm-lit -v test
#smoke: $(TARGET)
#	./nullness-c++ -S -emit-llvm -o /dev/null test/simple.cpp -v
