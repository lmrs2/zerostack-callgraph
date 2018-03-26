#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/LegacyPassManager.h"

#include "AnnotationInfo.h"

using namespace llvm;

namespace {

struct FunctionPointerAnno : public FunctionPass {

  static char ID;
  FunctionPointerAnno() : FunctionPass(ID) {}

  virtual void getAnalysisUsage(AnalysisUsage &Info) const {
    Info.addRequired<AnnotationInfo>();
  }

  virtual bool runOnFunction(Function &F) {
  
    AnnotationInfo &AI = getAnalysis<AnnotationInfo>();
    bool modified = false;
    Module & M = *F.getParent();
    LLVMContext &C = M.getContext();

    for (auto &BB : F) {
      for (auto &I : BB) {
        
        if (auto *CI = dyn_cast<CallInst>(&I)) {
    			
    			Value * V = CI->getCalledValue();
    		
    			StringRef Anno = AI.getAnnotation( V );
    			if ( Anno != "" ) {
    				//errs() << *V << " has annotation " << Anno << "\n";
    				// Also add the annotation to the call itself
    				MDNode* MD = MDNode::get(C, MDString::get(C, Anno)); assert (MD);
    				CI->setMetadata("tyann", MD);			

    			}
		    }
      }
    }
	
    return modified;
  }

};

}

char FunctionPointerAnno::ID = 0;

static void registerPass(const PassManagerBuilder &,
                         legacy::PassManagerBase &PM) {
  PM.add(new FunctionPointerAnno());
}

static RegisterStandardPasses
  RegisterMyPass(PassManagerBuilder::EP_EarlyAsPossible,
                 registerPass);
