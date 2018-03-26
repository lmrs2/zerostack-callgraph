#HERE := $(dir $(lastword $(MAKEFILE_LIST)))
#BUILD := $(HERE)/build
#BUILT := $(BUILD)/built
#BUILD := /auto/homes/lmrs2/zero_mem/LLVM/disk_LLVM/cse231-proj1/llvm/build
LLVM_CONFIG := llvm-config



# Get LLVM build parameters from its llvm-config program.
LLVM_CXXFLAGS := $(shell $(LLVM_CONFIG) --cxxflags) -fno-rtti
LLVM_LDFLAGS := $(shell $(LLVM_CONFIG) --ldflags)
LLVM_LIBS := $(shell $(LLVM_CONFIG) --libs --system-libs)
LLVM_LIB_DIR := $(shell $(LLVM_CONFIG) --libdir)
LLVM_BIN_DIR := $(shell $(LLVM_CONFIG) --bindir)
LLVM_SRC_DIR := $(shell $(LLVM_CONFIG) --src-root)
LLVM_BUILD_DIR := $(LLVM_SRC)/build
CLANG_LIBS := \
	-lclangAST \
	-lclangAnalysis \
	-lclangBasic \
	-lclangDriver \
	-lclangEdit \
	-lclangFrontend \
	-lclangFrontendTool \
	-lclangLex \
	-lclangParse \
	-lclangSema \
	-lclangEdit \
	-lclangASTMatchers \
	-lclangRewriteCore \
	-lclangRewriteFrontend \
	-lclangStaticAnalyzerFrontend \
	-lclangStaticAnalyzerCheckers \
	-lclangStaticAnalyzerCore \
	-lclangSerialization \
	-lclangTooling

# On OS X, you need to tell the linker that undefined symbols will be looked
# up at runtime.
PLUGIN_LDFLAGS := -shared
ifeq ($(shell uname -s),Darwin)
	PLUGIN_LDFLAGS += -Wl,-undefined,dynamic_lookup
endif

echo  lolo2=$LLVM_CXXFLAGS

# Platform-specific library suffix.
ifeq ($(shell uname -s),Darwin)
		LIBEXT := dylib
else
		LIBEXT := so
	echo lolo=$LLVM_CXXFLAGS
endif
