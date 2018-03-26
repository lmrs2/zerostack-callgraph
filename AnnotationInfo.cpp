#include "AnnotationInfo.h"
#include <llvm/IR/Constants.h>
#include <llvm/Support/raw_ostream.h>


using namespace llvm;

AnnotationInfo::AnnotationInfo() : ModulePass(ID) {}

bool AnnotationInfo::runOnModule(Module &M) {
	
	return false;
}

StringRef AnnotationInfo::getAnnotation(llvm::Value *V) {
	
	if (auto *I = dyn_cast<Instruction>(V)) {
		MDNode *MD = I->getMetadata("tyann");
		if (MD) {
			auto *MDS = cast<MDString>(MD->getOperand(0));
			return MDS->getString();
		}
	}
	return StringRef();
}

bool AnnotationInfo::hasAnnotation(Value *V, StringRef Ann, uint8_t level) {
  // Check instruction metadata.
  if (auto *I = dyn_cast<Instruction>(V)) {

    MDNode *MD = I->getMetadata("tyann");
    if (MD) {
		  //errs() << "Found annotation " << *MD << " for instruction '" << *V << "'\n";
      auto *MDS = cast<MDString>(MD->getOperand(0));
      if (MDS->getString().equals(Ann)) {
		  return true;
		  
		  
        auto *CAM = cast<ConstantAsMetadata>(MD->getOperand(1));
        auto *CI = cast<ConstantInt>(CAM->getValue());
        if (CI->getValue() == level) {
          return true;
        } else {
          return false;
        }
      }
    }
  }

  return false;
}

char AnnotationInfo::ID = 0;
static RegisterPass<AnnotationInfo> X("annotation-info",
                                      "gather type annotations",
                                      false,
                                      true);
