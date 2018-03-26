//#include "TypeAnnotations.h"

#include <clang/Frontend/MultiplexConsumer.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendAction.h>
#include <clang/Frontend/FrontendPluginRegistry.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Metadata.h>
#include <clang/AST/StmtVisitor.h>

#include <vector>
#include <stack> 

#define UNSUPPORTED_ARRAY_FUNCTION_POINTERS	"Unsupported: Arrays of objects containing annotated function pointers"
#define UNSUPPORTED_VAR_PARAM_FUNCTIONS	"Unsupported: Annotation for variable-parameter functions"
#define TYPE_ANNOTATION	"type_annotate_"	// we'll prepend this to any type annotations added to function declaration: this way we can recognize them in LLVM pass compared to other annotations

// In a production build, this would be passed as an option
// We can compile other libraries with ISLIBC=1, so long as there's no clash of function names
#define ISLIBC	1

using namespace clang;

/**
 * UNSUPPORTED:
 * 	- arrays of anything that contain annotated functions
 * 	- struct s = {.f1=0, .f2={1,2}}
 * 
 * 
 */
namespace {

// all taken from TypeAnnotation.h
class PluginAnnotation {

public:
	
	explicit PluginAnnotation(CompilerInstance & CI)
		: Context(CI.getASTContext()), CompInst(CI) {}
	
	// Note: I don't use this as I want the dev to annotate all
	// the function, ie i don't do inferrences of types
	template<class T>
	void AddAnnotation(T & t, StringRef A) const {
		
		auto *AT = llvm::dyn_cast<AnnotatedType>(t.getType());
		bool sameType = (AT && AT->getAnnotation() == A);
		if ( A.size() && !sameType ) {	
			t.setType(Context.getAnnotatedType(t.getType(), A));
		}
	}
	
	void AddAnnotation(QualType & QT, StringRef A) const {
		// this is jut to make a templated function work
	}
	
	llvm::StringRef AnnotationOf(const Type *T) const {
		// TODO multiple annotations?
		if (auto *AT = llvm::dyn_cast<AnnotatedType>(T)) {
			return AT->getAnnotation();
		} else {
			return llvm::StringRef();
		}
	}

	
	// =================================================================
	//
	//						Error emissions
	//
	// =================================================================
	template<class T>
	void EmitIncompatibleError(/*clang::Stmt*/ T * S, llvm::StringRef LAnno, llvm::StringRef RAnno) const {
	
		unsigned did = Diags().getCustomDiagID(
		DiagnosticsEngine::Error,
			"'%0' type incompatible with '%1' type"
		);
		
		Diags().Report(S->getLocStart(), did)
			<< (LAnno.size() ? LAnno : "unannotated")
			<< (RAnno.size() ? RAnno : "unannotated")
			<< CharSourceRange(S->getSourceRange(), false);
	}
	
	template<class T>
	void EmitNotSupportedWarning(/*clang::Stmt*/ T * S, llvm::StringRef Msg ) const {
	
		EmitNotSupported( S, Msg, DiagnosticsEngine::Warning );
	}
	
	template<class T>
	void EmitNotSupportedError(/*clang::Stmt*/ T * S, llvm::StringRef Msg ) const {
	
		EmitNotSupported( S, Msg, DiagnosticsEngine::Error );
	}
	
	template<class T>
	void EmitNotSupportedFatal(/*clang::Stmt*/ T * S, llvm::StringRef Msg ) const {
	
		EmitNotSupported( S, Msg, DiagnosticsEngine::Fatal );
	}
	
	template<class T>
	void EmitNotSupported(/*clang::Stmt*/ T * S, llvm::StringRef Msg, DiagnosticsEngine::Level level ) const {
	
		unsigned did = Diags().getCustomDiagID(
		level, "%0" );
		
		Diags().Report(S->getLocStart(), did)
			<< Msg
			<< CharSourceRange(S->getSourceRange(), false);
	}
		
	DiagnosticsEngine &Diags() const {	
		return CompInst.getDiagnostics();
	}

private:
	ASTContext & Context;
	CompilerInstance & CompInst;
};


class PluginExampleVisitor : public RecursiveASTVisitor<PluginExampleVisitor> {

public:
	explicit PluginExampleVisitor(CompilerInstance & CI)
		: Context(CI.getASTContext()), Annotator(CI), CurrentFunction(NULL) {}

	
	bool IsDecl( Expr * E ) const {
		QualType QT;
		return (0 != ExtractDecl(E, &QT) || QT != QualType());
	}
	
	// returns 0 means no declaration and can be ignored by caller
	// TODO: change function declaration. The pQT and FromIsDecl were after-thouhts to make it work...
	ValueDecl * ExtractDecl( Expr * E, QualType * pQT = NULL, bool FromIsDecl = false ) const {
		
		ValueDecl * ValueDecl = 0;
		
		for (;;) {
		
			assert ( E && "E is null" );
			
			if ( ValueDecl ) { break; }
			
			if ( MemberExpr * ME = llvm::dyn_cast_or_null<MemberExpr>(E) ) {
				// MemberExpr for structure and union members, need loop to remove, eg S.a.b.c until we find a DeclRefExpr
				// Note that we don't even need to call getBase repeateadly, as getMemberDecl gives us what we want in one line
				// we could also call getBase and then fall to the common case
				
				ValueDecl = ME->getMemberDecl ();
				assert ( ValueDecl && "VDcl null" );
				
			} else if ( DeclRefExpr * DRE = llvm::dyn_cast_or_null<DeclRefExpr>(E) ) {
			
				//llvm::errs() << "--- DeclRefExpr: \n";
				//DRE->dump();
				
				ValueDecl =  DRE->getDecl ();
				assert ( ValueDecl && "ValueDecl null" );
				
				
			} else if ( UnaryOperator * UO = llvm::dyn_cast_or_null<UnaryOperator>(E) ) {
				// this is for RHS, ie = &hash
				//llvm::errs() << "--- UnaryOperator: \n";
				//UO->dump();
				
				E = UO->getSubExpr();
				
			} else if ( CastExpr *CE = llvm::dyn_cast_or_null<CastExpr>(E) ) {
				// this is for RHS, eg = hash (auto cast to &hash)
				// also for = NULL
				//llvm::errs() << "--- CastExpr: \n";
				//CE->dump();
				
				E = CE->getSubExpr();
			
			} else if ( ParenExpr * PE = llvm::dyn_cast_or_null<ParenExpr>(E) ) {
				// = NULL can have a ParenExpr before the CastExpr above
				//llvm::errs() << "--- ParenExpr: \n";
				//PE->dump();
				E = PE->getSubExpr();
			
			} else if ( CallExpr * CE = llvm::dyn_cast_or_null<CallExpr>(E) ) {
				//llvm::errs() << "--- CallExpr: \n";
				//CE->dump();
				FunctionDecl *FDC = CE->getDirectCallee();
				if ( FDC ) { ValueDecl = FDC; }
				else { E = CE->getCallee(); }
				
			// NOTE: I explicitly check each possibility for now, to make sure i don't miss important ones
			// in the end, we should simly have a return 0 for the default case
			} else if ( isa<ArraySubscriptExpr>(E) ) {
				//llvm::errs() << "--- ArraySubscriptExpr: \n";
				
				QualType QT = ExtractArrayElementType( E );
							
				assert ( pQT && "pQT is null" );
					
				*pQT = QT;
				
				return 0;

			} else if ( isa<PredefinedExpr>(E) ) {
				
				//llvm::errs() << "--- PredefinedExpr: \n"; // eg __FUNCTION__
				return 0;
				
			} else if ( isa<VAArgExpr>(E) ) {
				
				//llvm::errs() << "--- VAArgExpr: \n";
				return 0;
			
			} else if ( isa<UnaryExprOrTypeTraitExpr>(E) ) {
				// happens, eg with sizeof()/alignof() and vec_step() for openCL
				//llvm::errs() << "--- UnaryExprOrTypeTraitExpr: \n";
				//UEOTTE->dump();
				//E = UEOTTE->getArgumentExpr();
				return 0; 

			} else if ( isa<BinaryOperator>(E) ) {
				// eg foo(param+N, param2+N2), or pFunc += 1 for pointer. This is always fine
				//llvm::errs() << "--- BinaryOperator: \n";
				return 0;

			} else if ( isa<StringLiteral>(E) ) {
				//llvm::errs() << "--- StringLiteral: \n";
				return 0;

			} else if ( isa<ImplicitValueInitExpr>(E) ) {
				// this is for struct = {0} or {}
				//llvm::errs() << "--- ImplicitValueInitExpr: \n";
				return 0; // this is always fine
				
			} else if ( isa<IntegerLiteral>(E) ) {
				// I don't check for 0 since this will already be warned by the compiler
				//llvm::APInt = IL->getValue().isIntN(0);
				//llvm::errs() << "--- IntegerLiteral: \n";
				return 0; // special case that means accept

			} else if ( isa<FloatingLiteral>(E) ) {
				//llvm::errs() << "--- FloatingLiteral: \n";
				return 0; // special case that means accept

			} else if ( isa<CharacterLiteral>(E) ) {
				//llvm::errs() << "--- CharacterLiteral: \n";
				return 0;

			} else if ( isa<OffsetOfExpr>(E) ) {
				//llvm::errs() << "--- OffsetOfExpr: \n"; // offsetof()
				return 0;

			} else {
				E->dump();
				assert (0 && "Not supported");
			}
		}
		
		return ValueDecl;
	}
	
	
	llvm::StringRef ExtractAnnotation ( const Type * T ) const {
	
		assert ( T && "T is null" );
		
		
		// if we already have an annotated type, simply return its annotation
		if ( const AnnotatedType * AT = llvm::dyn_cast_or_null<AnnotatedType>(T) ) {
			return AT->getAnnotation();
		}
		
		// must do this after the check for AnnotatedType, as this will remove the AnnotatedType
		T = T->getUnqualifiedDesugaredType();
		
		// if it's an array, just use the element info
		if ( const ArrayType * AT = llvm::dyn_cast_or_null<ArrayType>(T) ) {
			//llvm::errs() << "--- ArrayType: \n";
			return ExtractAnnotation( AT->getElementType() );
			// WARNING: this does not support multi-dimensional arrays...
		}
		
		// can be a typedef - taken care of by getUnqualifiedDesugaredType()
		/*
		if ( const TypedefType *TT = llvm::dyn_cast_or_null<TypedefType>(T) ) {
		
			llvm::errs() << "--- TypedefType: \n";
			TT->dump();
			
			T = TT->getDecl()->getUnderlyingType().getTypePtrOrNull();
			
			return ExtractAnnotation( T );
		}*/
		
		// Can be a pointer to a function directly, or one extracted from TypedefType
		if ( const PointerType * PT = llvm::dyn_cast_or_null<PointerType>(T) ) {
						
			return ExtractAnnotation( PT->getPointeeType() );
		}
		
		if ( const FunctionNoProtoType * FNPT = llvm::dyn_cast_or_null<FunctionNoProtoType>(T) ) {
			//llvm::errs() << "--- FunctionNoProtoType: \n";
			//FNPT->dump();
			T = FNPT->getReturnType().getTypePtrOrNull();
			
			//llvm::errs() << "Anno: " << Annotator.AnnotationOf(T) << "\n";
			
			// return value may be a pointer itself
			if ( T && (isa<TypedefType>(T) || isa<PointerType>(T)) ) { return ExtractAnnotation( T ); } 
			
			return Annotator.AnnotationOf(T);
		}
		
		
		// can be a function prototype directly, or one extracted from the PointerType
		if ( const FunctionProtoType * FPT = llvm::dyn_cast_or_null<FunctionProtoType>(T) ) {
			//llvm::errs() << "--- FunctionProtoType: \n";
			
			assert ( FPT && "LHS_Type is null" );
			//FPT->dump();
			
			T = FPT->getReturnType().getTypePtrOrNull();
			assert ( T && "T is null" );
			
			// return value may be a pointer itself
			if ( T && (isa<TypedefType>(T) || isa<PointerType>(T)) ) { return ExtractAnnotation( T ); } 
			
			//llvm::errs() << "Anno: " << Annotator.AnnotationOf(T) << "\n";
			
			return Annotator.AnnotationOf(T);
		}
		
		return llvm::StringRef();
	}
	
	llvm::StringRef ExtractAnnotation ( const QualType QT ) const {
		// here we dont want the underlying type since we want to catch the AnnotatedType!
		return ExtractAnnotation( /*ExtractUnderlyingType(QT)*/QT.getTypePtrOrNull() );
	}
	
	llvm::StringRef ExtractAnnotation( ValueDecl & VD ) const {
				
		return ExtractAnnotation( VD.getType() );
	}
	
	// these recursively try to find annotation in the object
	llvm::StringRef FindAnyAnnotationInArray ( const QualType QT, bool * ArrayFound, std::set<intptr_t> & seenFields  ) const {
	
		return FindAnyAnnotationInArray( QT.getTypePtrOrNull(), ArrayFound, seenFields );
	}
	
	llvm::StringRef FindAnyAnnotationInArray( ValueDecl & VD, bool * ArrayFound, std::set<intptr_t> & seenFields ) const {
				
		return FindAnyAnnotationInArray( VD.getType(), ArrayFound, seenFields );
	}
	
	// utility function to find if an item is in a set

	template<class T> inline
	bool Contains(const std::set<T>& container, const T& value) const
	{
		return container.find(value) != container.end();
	}
	
	llvm::StringRef FindAnyAnnotationInArray ( const Type * T, bool * ArrayFound, std::set<intptr_t> & seenFields ) const {
		
		// if we already have an annotated type, simply return its annotation
		if ( const AnnotatedType * AT = llvm::dyn_cast_or_null<AnnotatedType>(T) ) {
			if ( AT->getAnnotation() != "" && *ArrayFound ) { return AT->getAnnotation(); }
			T = AT->getBaseType().getTypePtrOrNull();
		}
		
		// if it's an array, just use the element info
		if ( const ArrayType * AT = llvm::dyn_cast_or_null<ArrayType>(T) ) {
			//llvm::errs() << "--- ArrayType: \n";
			T = AT->getElementType().getTypePtrOrNull();
			//T->dump();
			
			bool dummy = false; // use a different variable here so we don't inavertendly emit msg for things like struct{ int t[3]; ANNOTATED void (*func)(void); }
			llvm::StringRef Ret = FindAnyAnnotationInArray( T, &dummy, seenFields ); // support multi-dimensional arrays
			if (Ret != "") { *ArrayFound = true; } // set the flag only if there is annotation, so we dont get annotation "removed"
			return Ret;
		}
		
		// can be a typedef
		if ( const TypedefType *TT = llvm::dyn_cast_or_null<TypedefType>(T) ) {
		
			//llvm::errs() << "--- TypedefType: \n";
			//TT->dump();
			
			T = TT->getDecl()->getUnderlyingType().getTypePtrOrNull();
			
			return FindAnyAnnotationInArray(T, ArrayFound, seenFields);
			
		}
		
		if ( const ElaboratedType * ET = llvm::dyn_cast_or_null<ElaboratedType>(T) ) {
			//llvm::errs() << "--- ElaboratedType: \n";
			T = ET->desugar().getTypePtrOrNull();
			//T->dump();
		}
		
		// I think this works for both unions and structures
		if ( const RecordType * RT = llvm::dyn_cast_or_null<RecordType>(T) ) {
			//llvm::errs() << "--- RecordType: \n";
			//RT->dump();
			const RecordDecl * RD = RT->getDecl();
			for ( auto it = RD->field_begin(); it!= RD->field_end(); ++it ) {
				FieldDecl * FD = *it;
				
				// avoid inifinite loops, eg struct Node{ int val; struct Node * next; }
				if ( !Contains(seenFields, (intptr_t)FD) ) {
					seenFields.insert( (intptr_t)FD );
					StringRef Anno = FindAnyAnnotationInArray( *FD, ArrayFound, seenFields );
					if ( Anno != "" ) { return Anno; }
				}
				 
			}
		}
		
		// Can be a pointer to a function directly, or one extracted from TypedefType
		if ( const PointerType * PT = llvm::dyn_cast_or_null<PointerType>(T) ) {
			
			// get the function prototype
			//llvm::errs() << "--- getPointeeType: \n";
			T = PT->getPointeeType().getDesugaredType(Context).getTypePtrOrNull();
			assert ( T && "T is null" );
			
			return FindAnyAnnotationInArray(T, ArrayFound, seenFields);
		}
		
		if ( const FunctionNoProtoType * FNPT = llvm::dyn_cast_or_null<FunctionNoProtoType>(T) ) {
			//llvm::errs() << "--- FunctionNoProtoType: \n";
			//FNPT->dump();
			T = FNPT->getReturnType().getTypePtrOrNull();
			
			//llvm::errs() << "Anno: " << Annotator.AnnotationOf(T) << "\n";
			return Annotator.AnnotationOf(T);
		}
		
		
		// can be a function prototype directly, or one extracted from the PointerType
		if ( const FunctionProtoType * FPT = llvm::dyn_cast_or_null<FunctionProtoType>(T) ) {
			//llvm::errs() << "--- FunctionProtoType: \n";
			
			assert ( FPT && "LHS_Type is null" );
			//FPT->dump();
			
			T = FPT->getReturnType().getTypePtrOrNull();
			assert ( T && "T is null" );
			//T->dump();
			
			//llvm::errs() << "Anno: " << Annotator.AnnotationOf(T) << "\n";
			return Annotator.AnnotationOf(T);
		}
		
		return llvm::StringRef();
	}
	
	template<class T>
	bool IsStructure( T & t ) const {
		return IsStructure( t.getType() );
	}
	
	bool IsStructure( const Type * T ) const {
		if ( RecordDecl * RDL = ExtractRecord(T) ) { 
			return ! isa<BuiltinType>(RDL->getTypeForDecl());
		}
		return false;
	}
	
	bool IsStructure( QualType QT ) const {
		return IsStructure( ExtractUnderlyingType(QT).getTypePtrOrNull() );	// just added IgnoreParens - not tested
	}
	
	RecordDecl* ExtractRecord( const Type * T ) const {
		
		assert ( T && "T is null" );
				
		T = T->getUnqualifiedDesugaredType(); // this does the TypedefType and ElaboratedType, and does it recursively so we can handle struct s{ struct s2 }
		
		if ( const PointerType * PT = dyn_cast_or_null<PointerType>(T) ) {
			return ExtractRecord( PT->getPointeeType() ); // not tested
		}
		
		T = T->getUnqualifiedDesugaredType(); // this does the TypedefType and ElaboratedType, and does it recursively so we can handle struct s{ struct s2 }
		
		if ( const RecordType * RT = llvm::dyn_cast_or_null<RecordType>(T) ) {
			return const_cast<RecordDecl*>(RT->getDecl());
		}
		
		
		return 0;
	}
	
	RecordDecl* ExtractRecord( QualType QT ) const {
		return ExtractRecord( QT.IgnoreParens().getTypePtrOrNull() ); 
	}
	
	template<class T>
	RecordDecl* ExtractRecord( T & t ) const {
		return ExtractRecord( t.getType() ); // just added IgnoreParens - not tested
	}
	
	bool IsFunction( QualType QT ) const {
		return IsFunction( ExtractUnderlyingType(QT).getTypePtrOrNull() );
	}
	
	bool IsFunction( ValueDecl & VD ) const {		
		return IsFunction( VD.getType() );
	}
	
	bool IsFunction( const Type * T ) const {
		
		assert ( T && "T is null" );
		
		T = T->getUnqualifiedDesugaredType(); // this does the TypedefType and ElaboratedType, and does it recursively so we can handle struct s{ struct s2 }
				  
		if ( const AnnotatedType * AT = dyn_cast_or_null<AnnotatedType>(T) ) {
			return IsFunction( AT->getBaseType() );
		}
		
		
		if ( const PointerType * PT = llvm::dyn_cast_or_null<PointerType>(T) ) {
			
			T = PT->getPointeeType().getDesugaredType(Context).getTypePtrOrNull();
			assert ( T && "T is null" );
		}
		
		return ( isa<FunctionNoProtoType>(T) || isa<FunctionProtoType>(T) );
		
	}
	
	template<class CT> 
	CT * ExtractFunction( const QualType QT ) const {
		return ExtractFunction<CT>( ExtractUnderlyingType(QT).getTypePtrOrNull() );
	}
	
	template<class CT> 
	CT * ExtractFunction( ValueDecl & VD ) const {		
		return ExtractFunction<CT>( VD.getType() );
	}
	
	template<class CT> 
	CT * ExtractFunction( const Type * T ) const {
		
		assert ( T && "T is null" );
		
		T = T->getUnqualifiedDesugaredType();	// just added this - not much tested
		
		if ( const AnnotatedType * AT = dyn_cast_or_null<AnnotatedType>(T) ) {
			return ExtractFunction<CT>( AT->getBaseType() );
		}
		
		if ( const PointerType * PT = llvm::dyn_cast_or_null<PointerType>(T) ) {
			
			// get the function prototype
			T = PT->getPointeeType().getDesugaredType(Context).getTypePtrOrNull();
			assert ( T && "T is null" );
		}
			
		// can be a function prototype directly, or one extracted from the PointerType
		if ( const CT * FPT = llvm::dyn_cast_or_null<CT>(T) ) {			
			return const_cast<CT*>(FPT);
		}
		
		return 0;
		
	}
	
	FunctionProtoType * ExtractFunctionPrototype( const Type * T ) const {
		
		return ExtractFunction<FunctionProtoType>(T);
	}
	
	FunctionProtoType * ExtractFunctionPrototype( Expr & E ) const {
		return ExtractFunction<FunctionProtoType>( E.getType() );
	}
	
	FunctionProtoType * ExtractFunctionPrototype( const QualType QT ) const {
		return ExtractFunction<FunctionProtoType>( ExtractUnderlyingType(QT).getTypePtrOrNull() );
	}
	
	FunctionProtoType * ExtractFunctionPrototype( ValueDecl & VD ) const {		
		return ExtractFunction<FunctionProtoType>( VD.getType() );
	}
	
	bool IsFunctionNoPrototype( Expr & E ) const {
		return IsFunctionNoPrototype( E.getType() );
	}
	
	bool IsFunctionNoPrototype( const QualType QT ) const {
		return IsFunctionNoPrototype( ExtractUnderlyingType(QT).getTypePtrOrNull() );
	}
	
	bool IsFunctionNoPrototype( ValueDecl & VD ) const {
		return IsFunctionNoPrototype( VD.getType() );
	}
	
	bool IsFunctionNoPrototype( const Type * T ) const {
		return ( 0 != ExtractFunctionNoPrototype(T) );
	}
	
	FunctionNoProtoType * ExtractFunctionNoPrototype( const QualType QT ) const {
		return ExtractFunction<FunctionNoProtoType>( QT.getTypePtrOrNull() );
	}
	
	FunctionNoProtoType * ExtractFunctionNoPrototype( ValueDecl & VD ) const {		
		return ExtractFunction<FunctionNoProtoType>( VD.getType() );
	}
	
	FunctionNoProtoType * ExtractFunctionNoPrototype( const Type * T ) const {
		return ExtractFunction<FunctionNoProtoType>(T);
	}
	
	QualType ExtractArrayElementType(Expr * E) const {
				
		E = ExtractUnderlyingExpr( E ); assert (E);
		QualType QT = E->getType();

		// see function getBaseElementTypeUnsafe()
		while (const ArrayType *AT = QT.getTypePtrOrNull()->getAsArrayTypeUnsafe()) {
			QT = ExtractUnderlyingType(AT->getElementType());
		}
		return QT;
		
	}
	
	bool VisitFunctionDecl (FunctionDecl* FD) {
    
		// WANRING: this is important: it takes the annotation from the returned value of the function
		// and puts it on the function declaration itself. This way, we can query in LLVM pass from the Function in the runOnFunction()
		
		QualType RetQType = FD->getReturnType();
		// add the annotation to the return value
		llvm::StringRef Anno = ExtractAnnotation( RetQType );
		
		if ( Anno != "" ) {
			// prepend TYPE_ANNOTATION to the annotation
			FD->addAttr(AnnotateAttr::CreateImplicit(FD->getASTContext(), (Twine(TYPE_ANNOTATION) + Twine(Anno)).str() ));
		}
		
		return true;
	}
	
	// struct s; is a record.
	bool VisitRecordDecl( RecordDecl * RD ) {
		
		for ( auto it = RD->field_begin(); it != RD->field_end(); ++it ) {
			FieldDecl * FD = *it;
			// Note: this could be a structure itself
			StringRef FD_Anno = ExtractAnnotation( *FD );	// this gets the one by the dev 
			Annotator.AddAnnotation( *FD, FD_Anno );	
		}
		return true;
	}
		
	// this happens when struct s = X
	// this function used to take a VarDecl as parameters, and I would get
	// the Init_E thru getInit(). But to be able to call it recursively with
	// FieldEcl structures fields, I've moved to using ValueDecl and pass
	// the Init_E as parameters
	void ValidateStructureVarDeclInitializer( ValueDecl * VD, Expr * Init_E ) {
	
		assert ( VD && "VD is null" );
		assert ( IsStructure( *VD ) && "VD is not a structure" );
		
		
		if ( Init_E ) {
			
			Init_E = ExtractUnderlyingExpr( Init_E );
			
			if ( isa<ConditionalOperator>(Init_E) ) {
				
				ConditionalOperator * CO = llvm::dyn_cast_or_null<ConditionalOperator>(Init_E); assert (CO);
				Expr * TrueExpr = CO->getTrueExpr(); assert ( TrueExpr && "TrueExpr is null" );
				Expr * FalseExpr = CO->getFalseExpr(); assert ( FalseExpr && "FalseExpr is null" );
				
				ValidateStructureVarDeclInitializer( VD, TrueExpr );
				ValidateStructureVarDeclInitializer( VD, FalseExpr );
			
			} else if ( isa<BinaryOperator>(Init_E) ) {
				
				BinaryOperator * BO = llvm::dyn_cast_or_null<BinaryOperator>(Init_E); assert (BO);
				Expr * Left = BO->getLHS(); assert ( Left && "Left is null" );
				Expr * Right = BO->getRHS(); assert ( Right && "Right is null" );
				
				ValidateStructureVarDeclInitializer( VD, Left );
				ValidateStructureVarDeclInitializer( VD, Right );
			
				
			} else if ( isa<CompoundLiteralExpr>(Init_E) ) {
			
				// eg (struct S []){S1, S2}
				CompoundLiteralExpr * CLE = llvm::dyn_cast_or_null<CompoundLiteralExpr>(Init_E); assert (CLE);
				Expr * CLE_Init = CLE->getInitializer();
				
				ValidateStructureVarDeclInitializer( VD, CLE_Init );
			
			} else if ( isa<VAArgExpr>(Init_E) ) {
				
				VAArgExpr * VAAE = llvm::dyn_cast_or_null<VAArgExpr>(Init_E); assert (VAAE);
				ValidateStructureVarDeclInitializer( VD, VAAE->getSubExpr() );
				
			} else if ( !isa<InitListExpr>(Init_E) ) { 
				
				// Note: for now I check each possibiilty. In the end, we could just use if ( isa<blabla> ) do_stg
				// don't proceed if we're not initializing the function pointers directly
				// eg, could be initialized with a variable, in which case we've already validated the assignment/declaration
				if ( isa<ImplicitValueInitExpr>(Init_E) || isa<IntegerLiteral>(Init_E) ) { return; }	// = {0} // struct s = {0}
				if ( isa<OffsetOfExpr>(Init_E) ) { return; } // that's an integer, not a function pointer
				if ( isa<UnaryExprOrTypeTraitExpr>(Init_E) ) { return; }	// sizeof, etc see http://clang.llvm.org/doxygen/namespaceclang.html#a5d73f06594a5ccb763a726bed94a541f
				if ( IsDecl( Init_E ) ) { return; } // Note: this also works for CallExpr, arrays, etc
				//if ( isa<CallExpr>(Init_E) ) { return; }	// eg FILE * f = fopen() - this is always ok as the returned value is not a function pointer (it's a structure!)
				//if ( isa<MemberExpr>(Init_E) ) { return; }	// eg struct ee * psee; psee = &(pff->see); - this is always ok as it's not a function pointer (it's a structure!)
				
				//Init_E->dump();
				assert (CurrentFunction); 
				//llvm::errs() << CurrentFunction->getName() << "\n";
				assert ( 0 && "Invalid Init_E type" );
				
			} else {
					
				assert ( isa<InitListExpr>(Init_E) && "Init_E not a InitListExpr" );
								
				InitListExpr * ILE_Init = llvm::dyn_cast_or_null<InitListExpr>(Init_E); assert ( Init_E );
				
				// get the LHS's (structure's) fields
				// RecordDecl contains multiple FieldDecl
				RecordDecl* RVD = ExtractRecord(*VD); assert ( RVD && "RVD is null" );
				std::vector<FieldDecl*> VD_Fields;
				for ( auto it = RVD->field_begin(); it != RVD->field_end(); ++it ) {
					FieldDecl * FD = *it;
					//llvm::errs() << "FD:\n"; FD->dump();
					// the field MUST be named, otherwise initialization will have a different number of fields
					// eg struct s { int a; unsigned :bit_number; } where bit_number is used for alignment and might be null
					// eg in musl statvfs.c, it uses 
					// statvfs {
					//	...
					//	unsigned :8*(2*sizeof(int)-sizeof(long));
					//	...
					// }
					if ( FD->getName() != "" ) {
						VD_Fields.push_back( FD ); 
					}
				}
					
				// get the RHS's fields
				std::vector<Expr*> IE_Init_Fields;
				for ( Expr * IE_Init : ILE_Init->inits() ) {
					//llvm::errs() << "IE_Init:\n"; IE_Init->dump();
					IE_Init_Fields.push_back(IE_Init);
				}
				
				// now we have both sides, compares them
				// Note: some fields might contains the IntegerLiteral 0,
				// or some other structure:TODO
				
				//llvm::errs() << VD_Fields.size() << " " << IE_Init_Fields.size() << "\n";
					
				// sizes are always the same for structures, but not for unions!
				// even a struct = {} or {0} will appear to use with a cast of 0 for each field of the structure
				if ( VD_Fields.size() == IE_Init_Fields.size() ) {
				
					assert ( 0 == ILE_Init->getInitializedFieldInUnion() && "union with multiple field initialization");
					size_t i = 0;
					for ( i=0; i<VD_Fields.size(); ++i ) {
						
						FieldDecl *FD = VD_Fields[i];
						Expr * Init = IE_Init_Fields[i];
												
						if ( isa<IntegerLiteral>(Init) ) { continue; }
						
						// is it a itself structure?
						if ( IsStructure( *FD ) ) {
							ValidateStructureVarDeclInitializer( FD, Init );
						} else {
							// this is a "simple" field
							ValidateNonStructureVarDeclInitializer( FD, Init );
						}
					}
					
				} else {
					// this is an union, so more fields that used for initialization
					assert ( VD_Fields.size() > IE_Init_Fields.size() );
					if ( IE_Init_Fields.size() != 0 ) { // == 0 if union = {}
						
						assert ( IE_Init_Fields.size() == 1 );	// assume there's a single field for initialization...
						
						// get the initializer
						Expr * Init = IE_Init_Fields[0];
						
						
						FieldDecl * UnionField = ILE_Init->getInitializedFieldInUnion();
						assert ( UnionField && "UnionField is null" );
												
						size_t i = 0;
						bool Found = false;
						for ( i=0; i<VD_Fields.size(); ++i ) {
							
							FieldDecl *FD = VD_Fields[i];
							if ( FD == UnionField ) {
								Found = true;
								break;
							}
						}
						assert ( Found && "Could not find initialized field" );
						
						// is it a itself structure/union?
						if ( IsStructure( *UnionField ) ) {
							ValidateStructureVarDeclInitializer( UnionField, Init );
						} else {
							// this is a "simple" field
							ValidateNonStructureVarDeclInitializer( UnionField, Init );
						}	
					}				
				}
			}
		}
	}
	
		
	Expr * ExtractUnderlyingExpr( Expr * E ) const {
		for (;;) {
			if (E && isa<CastExpr>(E)) {
				CastExpr * CE = llvm::dyn_cast_or_null<CastExpr>(E); assert (CE);
				E = CE->getSubExpr();
			} else if (E && isa<ParenExpr>(E)) {
				ParenExpr * PE = llvm::dyn_cast_or_null<ParenExpr>(E); assert (PE);
				E = PE->getSubExpr();
			} else if ( E && isa<UnaryOperator>(E) ) {
				UnaryOperator * UO = llvm::dyn_cast_or_null<UnaryOperator>(E); assert (UO);
				E = UO->getSubExpr();
			} else {
				return E;
			}
		}
	}
	
	QualType ExtractUnderlyingType( QualType QT ) const {
		for (;;) {
			const Type * T = QT.getTypePtrOrNull();
			if ( const PointerType * PT = llvm::dyn_cast_or_null<PointerType>(T) ) {
				QT = PT->getPointeeType().IgnoreParens().getDesugaredType(Context); // use desugared here?
			} else {
				return QT;
			}
		}
	}
	
		
	template<class T>
	bool IsBuiltIn( T & t ) const {
		return IsBuiltIn( t.getType() );
	}
	
	bool IsBuiltIn( QualType QT ) const {
		return IsBuiltIn( ExtractUnderlyingType(QT).getTypePtrOrNull() );
	}
	
	bool IsBuiltIn( const Type * T ) const {
		return isa<BuiltinType>(T->getUnqualifiedDesugaredType());
	}
	
	
	// This used to accept a ValueDecl as parameters. See details 
	// at ValidateStructureVarDeclInitializer
	void ValidateNonStructureVarDeclInitializer( ValueDecl * VD, Expr * Init_E ) {
		
		assert ( VD && "VS is null" );
		assert ( !IsStructure( *VD ) && "VD is a structure" );
		
		const Type * VD_Orig_Type = VD->getType().getTypePtrOrNull(); assert ( VD_Orig_Type && "VD_Orig_Type is null" );
		const Type * VD_Type = ExtractUnderlyingType(VD->getType()).getTypePtrOrNull(); assert ( VD_Type && "VD_Type is null" );
		VD_Type = VD_Type->getUnqualifiedDesugaredType(); // this is important too
		
		// is this of type v =  a ? B : c. 
		Init_E = ExtractUnderlyingExpr(Init_E); // to handle nested ConditionalOperator like a ? ( d ? e : f) : c
		
		if ( Init_E && isa<ConditionalOperator>(Init_E) ) {
			
			ConditionalOperator * Init_CO = llvm::dyn_cast_or_null<ConditionalOperator>(Init_E); assert (Init_CO && "Init_CO is null"); // assertion impossible normally :)
			Expr * TrueExpr = Init_CO->getTrueExpr(); assert ( TrueExpr && "TrueExpr is null" );
			Expr * FalseExpr = Init_CO->getFalseExpr(); assert ( FalseExpr && "FalseExpr is null" );
			
			ValidateNonStructureVarDeclInitializer( VD, TrueExpr ); 
			ValidateNonStructureVarDeclInitializer( VD, FalseExpr ); 
		
		} else if ( Init_E && isa<CompoundLiteralExpr>(Init_E) ) {
			
			// eg (struct S []){S1, S2}
			CompoundLiteralExpr * CLE = llvm::dyn_cast_or_null<CompoundLiteralExpr>(Init_E); assert (CLE);
			Expr * CLE_Init = CLE->getInitializer();
			
			ValidateNonStructureVarDeclInitializer( VD, CLE_Init );
		
		} else if ( Init_E && isa<ArrayType>(VD_Type) ) {
						
			const ArrayType * VD_AT = llvm::dyn_cast<ArrayType>(VD_Type);
			QualType VD_Elt_QT = VD_AT->getElementType();
			
			// bail out if we're dealing with builtins, eg char t[] = "HelloWorld"
			if ( IsBuiltIn( VD_Elt_QT ) ) { return; }
			
			// if it's not an InitListExpr, I think we're assigning to an array a previously-declared variable. So we should be fine because the compiler should warn us if types are incompatible. 
			// I prefer adding some explicit checks for now
			// the problem occured in musl's dynlink.c when doing "rtld_fail = &jb;"
			if ( !isa<InitListExpr>(Init_E) && isa<PointerType>(VD_Orig_Type) && IsStructure( VD_Elt_QT ) ) { return; }		
			
			assert ( isa<InitListExpr>(Init_E) && "ArrayType not initialized with InitListExpr" );
						
			InitListExpr * ILE_Init = llvm::dyn_cast_or_null<InitListExpr>(Init_E); assert (ILE_Init);
			
			// get the function prototype from declared variable
			FunctionProtoType * VD_FP = ExtractFunctionPrototype( VD_Elt_QT ); 
			
			if ( !VD_FP ) {
								
				// this could be anything: (multi-)dimensional array of struct/union, etc
				bool isStruct = IsStructure( VD_Elt_QT );
				
				for ( Expr * IE_Init : ILE_Init->inits() ) {
					
					if (isStruct) { ValidateStructure( IE_Init, VD_Elt_QT ); }
					else          { ValidateNonStructure( IE_Init, VD_Elt_QT ); }
				}
				
			} else {
				
				// extract the initialiazation for each element
				for ( Expr * IE_Init : ILE_Init->inits() ) {
					// Note: we must NOT extract a function prototype from IE_Init, as it may itself be a compound/conditionaloperator, etc
					ValidateFunctionPrototypes( IE_Init, VD_FP, IE_Init ); 
				}
			}
			
		} else {
		
			// we must extract the annotation that is burried inside the return-value of the function pointer
			// it may also be behind a typedef, etc
			llvm::StringRef VD_Anno = ExtractAnnotation( *VD );
			
			if (Init_E) {
				
				// we dont care about these, as they are not function pointer anyway
				// in fact, this is NECESSARY to avoid warning in int a = foo(); with ANNO int foo(){} since we take the return value for annotation purposes
				if ( IsBuiltIn( VD_Type ) ) { return; }
								
				assert ( ! isa<InitListExpr>(Init_E) && "Init_E is a InitListExpr" );
				
				QualType QT;
				ValueDecl *Init_VD = ExtractDecl(Init_E, &QT); 
				
				if ( Init_VD ) {
					
					ValidateNonStructureHelper(Init_E, *VD, *Init_VD);
					
				} else if ( QT != QualType() ) {
					
					// this should be a non-structure type
					assert ( !IsStructure(QT) && "QT is a structure" );
					
					ValidateNonStructureHelper(Init_E, *VD, QT);
				}
				
			}
			
			Annotator.AddAnnotation(*VD, VD_Anno);
			
		}
		
	}
	
	
	template<class S, class T>
	bool ValidateAnyAnnotation(S * s, T t) {
		
		// for now I don't support arrays of function pointers, array of structure containing function pointers, etc
		// Need to mix up code for structure, union and arrays in one function for this. Maybe can use array filler function from InitListExpr?
		// TODO:
				
		bool Ret = true;
		bool ArrayFound = false;
		std::set<intptr_t> SeenFields;
		StringRef Anno = FindAnyAnnotationInArray( t, &ArrayFound, SeenFields );
		if ( ArrayFound && Anno != "" ) {
			Annotator.EmitNotSupportedFatal(s, UNSUPPORTED_ARRAY_FUNCTION_POINTERS);
			Ret = false;
		}
		return Ret;
	}
	
	bool ValidateAnyAnnotation(VarDecl * VD) {
		
		// for now I don't support arrays of function pointers, array of structure containing function pointers, etc
		// Need to mix up code for structure, union and arrays in one function for this. Maybe can use array filler function from InitListExpr?
		// TODO:
				
		bool Ret = true;
		bool ArrayFound = false;
		std::set<intptr_t> SeenFields;
		StringRef Anno = FindAnyAnnotationInArray( *VD, &ArrayFound, SeenFields );
		if ( ArrayFound && Anno != "" ) {
			Annotator.EmitNotSupportedFatal(VD, UNSUPPORTED_ARRAY_FUNCTION_POINTERS);
			Ret = false;
		}
		return Ret;
	}
	
	
	// var a; var a = 0;
	bool VisitVarDecl( VarDecl * VD ) {
				
		Expr * Init = VD->getInit();
		
		// TODO: support arrays both decl and assign
		if ( IsStructure(*VD) ) {
			ValidateStructureVarDeclInitializer( VD, Init );
		} else {
			ValidateNonStructureVarDeclInitializer( VD, Init );
		}
		
		return true;
	}


	void ValidateStructureReturnStmt( Expr * Ret_E, QualType RetType ) {
		
		ValidateStructure( Ret_E, RetType );
	}

	template<class T>
	void ValidateStructure( Expr * Ret_E, T & t ) {
		ValidateStructure( Ret_E, t.getType() );
	}
	
	
	// Note: this is similar to ValidateStructureVarDeclInitializer()
	// TODO: merge both functions
	void ValidateStructure( Expr * Ret_E, QualType RetType ) {
				
		if ( Ret_E ) {
			
			
			Ret_E = ExtractUnderlyingExpr( Ret_E );
			RetType = ExtractUnderlyingType( RetType );
			
			if ( !IsStructure( /*RetType*/ *Ret_E ) ) { // need this, eg when return 0 for a pointer to struct
			
				ValidateNonStructure( Ret_E, RetType );
			
			} else 	if ( isa<ConditionalOperator>(Ret_E) ) {
				
				ConditionalOperator * CO = llvm::dyn_cast_or_null<ConditionalOperator>(Ret_E); assert (CO);
				Expr * TrueExpr = CO->getTrueExpr(); assert ( TrueExpr && "TrueExpr is null" );
				Expr * FalseExpr = CO->getFalseExpr(); assert ( FalseExpr && "FalseExpr is null" );
				
				ValidateStructure( TrueExpr, RetType );
				ValidateStructure( FalseExpr, RetType );
			
			} else if ( isa<BinaryOperator>(Ret_E) ) {
				
				BinaryOperator * BO = llvm::dyn_cast_or_null<BinaryOperator>(Ret_E); assert (BO);
				Expr * Left = BO->getLHS(); assert ( Left && "Left is null" );
				Expr * Right = BO->getRHS(); assert ( Right && "Right is null" );
				
				ValidateStructure( Left, RetType );
				ValidateStructure( Right, RetType );
			
				
			} else if ( isa<CompoundLiteralExpr>(Ret_E) ) {
							
				// eg (struct S []){S1, S2}
				CompoundLiteralExpr * CLE = llvm::dyn_cast_or_null<CompoundLiteralExpr>(Ret_E); assert (CLE);
				Expr * CLE_Init = CLE->getInitializer();
				//CLE_Init->dump();
				ValidateStructure( CLE_Init, RetType );
			
				
			} else if ( !isa<InitListExpr>(Ret_E) ) {
				
				//if ( isa<CallExpr>(Ret_E) ) { return; }	// eg FILE * f = fopen() - this is always ok as the returned value is not a function pointer (it's a structure!)
				//if ( isa<MemberExpr>(Ret_E) ) { return; }	// eg struct ee * psee; psee = &(pff->see); - this is always ok as it's not a function pointer (it's a structure!)
				if ( isa<ImplicitValueInitExpr>(Ret_E) || isa<IntegerLiteral>(Ret_E) ) { return; }	// = {0}
				// previously-declared variable: in this case we've validated the assignment/declaration
				// nothing to do as the compiler should have complained that structures are different
				// if the dev purposely casts, then we're screwed
				if ( IsDecl( Ret_E ) ) { return; }	// this also accounts for arrays[x], calls, etc
				assert ( 0 && "Invalid Ret_E type" );
				
			} else {
				
				assert ( isa<InitListExpr>(Ret_E) && "Ret_E not a InitListExpr" );
				
				InitListExpr * ILE_Ret = llvm::dyn_cast<InitListExpr>(Ret_E); 
				
				std::vector<Expr*> Ret_Fields;
				for ( Expr * Ret_Init : ILE_Ret->inits() ) {
					Ret_Fields.push_back(Ret_Init);
				}
				
				// it's possible a structure's address is cast to an pointer aka uintptr_t
				// eg __syscall (uintptr_t addr) -> __syscall(&(struct stat){0});
				// this occurs in tempnam.c of musl
				// in this case, just skip... we cannot keep track of these.
				if ( IsBuiltIn( RetType ) ) { return; }
				
				RecordDecl* Type_RD = ExtractRecord( RetType/*.getTypePtrOrNull()*/ ); assert ( Type_RD && "Type_RD is null" );
				std::vector<QualType> Type_Fields;
				for ( auto it = Type_RD->field_begin(); it != Type_RD->field_end(); ++it ) {
					FieldDecl * FD = *it;
					Type_Fields.push_back( FD->getType() );
				}
								
				// sizes are always the same for structures, but not for unions!
				// even a struct = {} or {0} will appear to use with a cast of 0 for each field of the structure
				if ( Ret_Fields.size() == Type_Fields.size() ) {
					
					size_t i = 0;
					for ( i=0; i<Ret_Fields.size(); ++i ) {
						
						Expr *Ret_FD = Ret_Fields[i];
						QualType QT = Type_Fields[i];
												
						bool Ret_struct = IsStructure( *Ret_FD );
						bool Type_struct = IsStructure( QT );
						
						assert ( (Ret_struct && Type_struct) || (!Ret_struct && !Type_struct) );
						
						// is it a itself structure?
						if ( Ret_struct ) {
							ValidateStructure( Ret_FD, QT );
						} else {
							// this is a "simple" field
							ValidateNonStructure( Ret_FD, QT );
						}
					}
				} else { 
					
					assert ( Ret_Fields.size() < Type_Fields.size() ); // unions have more fields than initialized
					
					if ( Ret_Fields.size() != 0 ) { // == 0 if union = {}
						
						assert ( Ret_Fields.size() == 1 );	// assume there's a single field for initialization...
					
						// get the initializer
						Expr * Ret_Init = Ret_Fields[0];
												
						FieldDecl * UnionField = ILE_Ret->getInitializedFieldInUnion();
						assert ( UnionField && "UnionField is null" );
						
						if ( IsStructure( * UnionField ) ) {
							ValidateStructure( Ret_Init, UnionField->getType() );
						} else {
							ValidateNonStructure( Ret_Init, UnionField->getType() );
						}
					}
				}
				
			}

		}
	}
	
	void ValidateNonStructureReturnStmt( Expr * Ret_E, QualType RetType ) {
		ValidateNonStructure( Ret_E, RetType );
	}
	
	template<class T>
	void ValidateNonStructure( Expr * Ret_E, T & t ) {
		ValidateNonStructure( Ret_E, t.getType() );
	}
	
	
	// Note: this is similar to ValidateNonStructureVarDeclInitializer()
	// TODO: merge both functions
	void ValidateNonStructure( Expr * Ret_E, QualType RetType ) {
		
		if ( Ret_E ) {
		
			Ret_E = ExtractUnderlyingExpr( Ret_E );
			RetType = ExtractUnderlyingType( RetType );
						
			// now that we got the unerlying expr, it may be a structure - eg originally we had a pointer to a structure
			if ( IsStructure( *Ret_E ) ) {
			
				ValidateStructure( Ret_E, RetType );
			
			} else 	if ( isa<ConditionalOperator>(Ret_E) ) {
				
				ConditionalOperator * CO = llvm::dyn_cast_or_null<ConditionalOperator>(Ret_E); assert (CO);
				Expr * TrueExpr = CO->getTrueExpr(); assert ( TrueExpr && "TrueExpr is null" );
				Expr * FalseExpr = CO->getFalseExpr(); assert ( FalseExpr && "FalseExpr is null" );
				
				ValidateNonStructure( TrueExpr, RetType );
				ValidateNonStructure( FalseExpr, RetType );
			
			} else if ( isa<CompoundLiteralExpr>(Ret_E) ) {
			
				// eg (struct S []){S1, S2}
				CompoundLiteralExpr * CLE = llvm::dyn_cast_or_null<CompoundLiteralExpr>(Ret_E); assert (CLE);
				Expr * CLE_Init = CLE->getInitializer();
				
				ValidateNonStructure( CLE_Init, RetType ); 
			
			} else if ( RetType.getTypePtrOrNull() && isa<ArrayType>( RetType.getTypePtrOrNull() ) ) {
								
				const ArrayType * AT = llvm::dyn_cast<ArrayType>( RetType.getTypePtrOrNull() );
				QualType QT = ExtractUnderlyingType(AT->getElementType()); //.IgnoreParens();
				
				// bail out if we're dealing with builtins, eg char t[] = "HelloWorld"
				if ( IsBuiltIn( QT ) ) { return; }
								 
				//Ret_E->dump();
				assert ( isa<InitListExpr>(Ret_E) && "ArrayType not initialized with InitListExpr" );
								
				InitListExpr * ILE_Ret_E = llvm::dyn_cast_or_null<InitListExpr>(Ret_E); assert (ILE_Ret_E);
				
				// get the function prototype
				FunctionProtoType * FP = ExtractFunctionPrototype( QT ); 
				
				if ( !FP ) {
					
					// this could be anything: (multi-)dimensional array of struct/union, etc
					bool isStruct = IsStructure( QT );
					
					for ( Expr * IE_Ret_E : ILE_Ret_E->inits() ) {
						
						if (isStruct) { ValidateStructure( IE_Ret_E, QT ); }
						else          { ValidateNonStructure( IE_Ret_E, QT ); }
					}
					
				} else {
					
					// extract the initialiazation for each element
					for ( Expr * IE_Ret_E : ILE_Ret_E->inits() ) {
						//llvm::errs() << "IE_Ret_E:\n"; IE_Ret_E->dump();
						FunctionProtoType * FP_IE_Ret_E = ExtractFunctionPrototype( *IE_Ret_E ); assert (FP_IE_Ret_E && "FP_IE_Ret_E is null");
						ValidateFunctionPrototypes( IE_Ret_E, FP, FP_IE_Ret_E ); 
					}
				}
			
			} else if ( isa<InitListExpr>(Ret_E) ) {
			
				//eg initialize a pointer from an array of struct defined as (struct S []){S1, S2}
				//and the RetType is NOT an array itself , ie eg, a function pointer...
				
				InitListExpr * ILE_Ret_E = llvm::dyn_cast_or_null<InitListExpr>(Ret_E); assert (ILE_Ret_E);
				
				for ( Expr * IE_Ret_E : ILE_Ret_E->inits() ) {					
					ValidateNonStructure( IE_Ret_E, RetType );
				}
				
				
			} else {
			
				
				QualType QT;
				ValueDecl *Ret_VD = ExtractDecl(Ret_E, &QT); 
				if ( Ret_VD ) {
					
					ValidateNonStructureHelper( Ret_E, *Ret_VD, RetType );
						
				} else if ( QT != QualType() )  {
									
					ValidateNonStructureHelper( Ret_E, QT, RetType );
				}
									
			}
		
		}
	}
	
	template<class T, class R>
	void ValidateNonStructureHelper( Expr * Ret_E, /*ValueDecl **/ T & Ret_VD, /*QualType*/ R & RetType ) {
			
		// check if we have no info on params - foo() or foo(blabla, ...)
		if ( IsFunctionNoPrototype( Ret_VD ) ) {
			Annotator.EmitNotSupportedError(Ret_E, UNSUPPORTED_VAR_PARAM_FUNCTIONS);
			return;
		}
		
		assert ( !IsFunctionNoPrototype( RetType ) && "WTF" );
		
		StringRef Ret_Anno = ExtractAnnotation(Ret_VD);
						
		StringRef CF_Ret_Anno = ExtractAnnotation( RetType );
		
		if ( CF_Ret_Anno != Ret_Anno && IsFunction( Ret_VD ) && IsFunction( RetType ) ) {
			Annotator.EmitIncompatibleError(Ret_E, Ret_Anno, CF_Ret_Anno);
		
		} else {
		
			Annotator.AddAnnotation(Ret_VD, Ret_Anno);
		
			// if return variable is a function pointer, also validate its parameters which may themselves be function pointers			
			ValidateFunctionPrototypes( Ret_E, ExtractFunctionPrototype(RetType), ExtractFunctionPrototype(Ret_VD) );
		}
	}

	// return X
	bool VisitReturnStmt(ReturnStmt *S) {
		
		assert ( CurrentFunction && "CurrentFunction is null" );
		
		Expr * Ret_E = S->getRetValue();
		if ( Ret_E ) { // can be null if within a void-return function, we have just "return;
							
			assert ( CurrentFunction && "CurrentFunction is null" );
								
			if ( IsStructure( Ret_E->getType() ) ) {
				ValidateStructure( Ret_E, CurrentFunction->getReturnType() );
			} else {
				ValidateNonStructure( Ret_E, CurrentFunction->getReturnType() );
			}
		}
		
		return true;
	}
	
	
	void ValidateNoAnnotationOnFunctionPrototype( Expr * E, FunctionProtoType * FTP, StringRef Msg ) const {
		if ( FTP ) {
			StringRef Anno = ExtractAnnotation( FTP );
			if ( Anno != "" ) {
				Annotator.EmitNotSupportedError(E, Msg);
				return; // we're done
			}
			
			size_t i = 0;
			for ( i=0; i<FTP->getNumParams(); ++i ) {
				QualType FTP_Type = FTP->getParamType(i);
				
				Anno = ExtractAnnotation(FTP_Type );
				
				if ( Anno != "" ) {
					// we found the first parameters with an annotation
					Annotator.EmitNotSupportedError(E, Msg);
					break;
				} else {
					// recurse on the params themselves
					ValidateNoAnnotationOnFunctionPrototype( E, ExtractFunctionPrototype(FTP_Type), Msg );
				}
			}
		}
	}
	
	template<class T>
	void ValidateFunctionPrototypes( T * t, FunctionProtoType * LeftFPT, Expr * E) const {
				
		if ( isa<ConditionalOperator>(E) ) {
			
			ConditionalOperator * CO = llvm::dyn_cast_or_null<ConditionalOperator>(E); assert (CO);
			Expr * TrueExpr = CO->getTrueExpr(); assert ( TrueExpr && "TrueExpr is null" );
			Expr * FalseExpr = CO->getFalseExpr(); assert ( FalseExpr && "FalseExpr is null" );
			
			ValidateFunctionPrototypes( TrueExpr, LeftFPT, TrueExpr );
			ValidateFunctionPrototypes( FalseExpr, LeftFPT, FalseExpr );
		
		} else if ( isa<CompoundLiteralExpr>(E) ) {
			assert (0 && "Not tested yet");
			// eg (struct S []){S1, S2}
			CompoundLiteralExpr * CLE = llvm::dyn_cast_or_null<CompoundLiteralExpr>(E); assert (CLE);
			Expr * CLE_Init = CLE->getInitializer();
			
			ValidateFunctionPrototypes( CLE_Init, LeftFPT, CLE_Init );
			
		} else {
			
			if ( IsFunctionNoPrototype( *E ) ) {
				Annotator.EmitNotSupportedError(t, UNSUPPORTED_VAR_PARAM_FUNCTIONS);
			} else {
				FunctionProtoType * RightFPT = ExtractFunctionPrototype( *E );
				ValidateFunctionPrototypes( t, LeftFPT, RightFPT );
			}
			
		}
	}
	
	template<class T>
	void ValidateFunctionPrototypes( /*Expr*/ T * E, FunctionProtoType * LeftFTP, FunctionProtoType * RightFTP) const {
	
		if ( LeftFTP && RightFTP ) {
			
			if ( LeftFTP != RightFTP ) {
								
				// left side has parameters, but the right side does not
				// I found that this to occur when we have a function that returns a function pointer and we do:
				// TYPE fp = returnFP() / fp = returnFP2()
				// `-FunctionProtoType 0x174a8a0 'TYPE (void)' cdecl <- this is the bit we want to strip off
				//  `-TypedefType 0x174a7e0 'TYPE' sugar
				//	|-Typedef 0x1747be0 'TYPE'
				if ( RightFTP->getNumParams() == 0 ) {
					// WARNING: this could loop for ever...
					ValidateFunctionPrototypes( E, LeftFTP, ExtractFunctionPrototype( RightFTP->getReturnType()/*.getTypePtrOrNull()*/ ) );
					return;
				}
				
#if ISLIBC
				// this function casts a function with a fixed number of params into one that can take any
				// and it calls the original function with one more argument. That's a hack we should not see
				// in other projects I hope
				//assert ( CurrentFunction && "CurrentFunction is null" );
				if ( CurrentFunction && CurrentFunction->getName() == "ftw" && Context.getSourceManager().getFilename( E->getExprLoc() ) == "src/legacy/ftw.c" ) { return; }
				
#endif	
				// get annotation for the function itself
				StringRef RightFTP_Anno = ExtractAnnotation( RightFTP );
				StringRef LeftFTP_Anno = ExtractAnnotation( LeftFTP );
				
				if ( RightFTP_Anno != LeftFTP_Anno && IsFunction( LeftFTP ) && IsFunction( RightFTP ) ) {
					Annotator.EmitIncompatibleError(E, RightFTP_Anno, LeftFTP_Anno);
					return; // we're done
				}
				
				// I found this to be possible if we have a function and a typedef function
				// void (*f)(void) and typedef void (*FType)(void). The second will have a no parameter and returns the function pointer
				if ( LeftFTP->getNumParams() != RightFTP->getNumParams() ) {
					
					assert ( ( LeftFTP->getNumParams() == 0 || RightFTP->getNumParams() == 0 ) && "Different number of parameters");
					FunctionProtoType ** FP1 = (LeftFTP->getNumParams() == 0) ? &LeftFTP : &RightFTP;
					do {
						
						assert ( (*FP1)->getNumParams() == 0 && "Different number of parameters");
						Type * T = ExtractFunctionPrototype( (*FP1)->getReturnType() ); assert ( T && "T not a FunctionProtoType" );
						(*FP1) = llvm::dyn_cast<FunctionProtoType>(T);
						
						
					} while( LeftFTP->getNumParams() != RightFTP->getNumParams() ); // this could loop for ever.. oops
					
				} 
				
				
				// annotations differ on some parameters, find the first difference so we can print a helpful error message
				assert ( LeftFTP->getNumParams() == RightFTP->getNumParams() && "Different number of parameters" ); // maybe relax in case not same param at all, ie dev mistake
				
				
				// if function have the same annotations, check the parameters themselves
				size_t i = 0;
				for ( i=0; i<LeftFTP->getNumParams(); ++i ) {
					QualType LeftFTP_Type = LeftFTP->getParamType(i);
					QualType RightFTP_Type = RightFTP->getParamType(i);
					
					LeftFTP_Anno = ExtractAnnotation(LeftFTP_Type );
					RightFTP_Anno = ExtractAnnotation( RightFTP_Type );
					if ( LeftFTP_Anno != RightFTP_Anno && IsFunction( LeftFTP_Type ) && IsFunction( RightFTP_Type ) ) {
						// we found the first parameters with different annotations
						Annotator.EmitIncompatibleError(E, RightFTP_Anno, LeftFTP_Anno);
						break;
					} else {
						// recurse on the params themselves
						ValidateFunctionPrototypes( E, ExtractFunctionPrototype(LeftFTP_Type), ExtractFunctionPrototype(RightFTP_Type) );
					}
				}
				
			}
				
		}
		
	}
	
	template<class TCA, class TPA>
	void ValidateCallExprParams( TCA CA, TPA PA) {
			
		// Note : we dont need to handle structures as parameters, since these will be flagged by the compiler
		Expr * CA_SC = ExtractUnderlyingExpr( CA );
		
		if ( CA_SC && isa<ConditionalOperator>(CA_SC) ) {
			ConditionalOperator * CO = llvm::dyn_cast_or_null<ConditionalOperator>(CA_SC); assert (CO);
			Expr * TrueExpr = CO->getTrueExpr(); assert ( TrueExpr && "TrueExpr is null" );
			Expr * FalseExpr = CO->getFalseExpr(); assert ( FalseExpr && "FalseExpr is null" );
			
			ValidateCallExprParams( TrueExpr, PA );
			ValidateCallExprParams( FalseExpr, PA );
		
		} else if ( CA_SC && isa<CompoundLiteralExpr>(CA_SC) ) {
			
			// eg (struct S []){S1, S2}
			CompoundLiteralExpr * CLE = llvm::dyn_cast_or_null<CompoundLiteralExpr>(CA_SC); assert (CLE);
			Expr * CLE_Init = CLE->getInitializer();
			
			if ( IsStructure(PA) ) {
				ValidateStructure(CLE_Init, PA);
			} else {
				ValidateNonStructure(CLE_Init, PA);
			}
			//ValidateCallExprParams( CLE_Init, PA );
			
		} else {
		
			StringRef Parm_anno = ExtractAnnotation( PA ); 
			
			QualType QT;
			ValueDecl *VArg = ExtractDecl( CA, &QT ); 
			if ( VArg ) {
				
				ValidateCallExprParamsHelper( CA, PA, *VArg, Parm_anno );
				
			} else if ( QT != QualType() ) {
				
				ValidateCallExprParamsHelper( CA, PA, QT, Parm_anno );
			}
						
		}

	}
	
	template<class TCA, class TPA, class T>
	void ValidateCallExprParamsHelper( TCA CA, TPA PA, T & t, StringRef Parm_anno) {
		
		StringRef Arg_anno = ExtractAnnotation( t );
		if ( Arg_anno != Parm_anno && IsFunction( t ) && IsFunction( PA ) ) {
			Annotator.EmitIncompatibleError(CA, Parm_anno, Arg_anno);
		} else {
						
			// if the parameter is a function, validate its parameters recursively
			ValidateFunctionPrototypes( CA, ExtractFunctionPrototype( t ), ExtractFunctionPrototype( PA ) );
		}
		
	}
	
	void ValidateCallExprParamsAlone( Expr * E ) {
	
		Expr * E_SC = ExtractUnderlyingExpr( E );
		
		if ( E_SC && isa<ConditionalOperator>(E_SC) ) {
			ConditionalOperator * CO = llvm::dyn_cast_or_null<ConditionalOperator>(E_SC); assert (CO);
			Expr * TrueExpr = CO->getTrueExpr(); assert ( TrueExpr && "TrueExpr is null" );
			Expr * FalseExpr = CO->getFalseExpr(); assert ( FalseExpr && "FalseExpr is null" );
			
			ValidateCallExprParamsAlone( TrueExpr );
			ValidateCallExprParamsAlone( FalseExpr );
		
		} else if ( E_SC && isa<CompoundLiteralExpr>(E_SC) ) {
			
			// eg (struct S []){S1, S2}
			//CompoundLiteralExpr * CLE = llvm::dyn_cast_or_null<CompoundLiteralExpr>(CA_SC); assert (CLE);
			//Expr * CLE_Init = CLE->getInitializer();
			assert (0);
			// no need to parse the initializer part, E_SC is simpler and faster
			//ValidateAnyAnnotation( E_SC, E_SC->getType() );
			
		} else {
			
			QualType QT;
			ValueDecl *VArg = ExtractDecl( E, &QT ); 
			
			if ( VArg ) {
				
				ValidateCallExprParamsAloneHelper(E, *VArg);
				
			} else if ( QT != QualType() ) {
				
				ValidateCallExprParamsAloneHelper(E, QT);
			}
		}
	}
	
	template<class T>
	void ValidateCallExprParamsAloneHelper( Expr * E, T & t ) {
		
		StringRef Arg_anno = ExtractAnnotation( t );
		if ( Arg_anno != "" ) {
			Annotator.EmitNotSupportedError(E, UNSUPPORTED_VAR_PARAM_FUNCTIONS);
		} else {
			
			// if the parameter is a function, validate its parameters recursively
			ValidateNoAnnotationOnFunctionPrototype( E, ExtractFunctionPrototype( t ), UNSUPPORTED_VAR_PARAM_FUNCTIONS );
		}		
	}
	
	// foo(n0, n1, etc)
	bool VisitCallExpr(CallExpr *CE) {
				
		// Check parameter types.
		FunctionDecl *FDC = CE->getDirectCallee();
		if ( FDC ) {
			// direct call: check the parameters passed
						
			if (FDC->param_size() == CE->getNumArgs()) {
								
				auto pi = FDC->param_begin();
				auto ai = CE->arg_begin();
				
				// Note : we dont need to handle structure as parameters, since these will be flagged by the compiler
				for (; pi != FDC->param_end() && ai != CE->arg_end(); ++pi, ++ai) {
					//(*ai)->dump();
					//(*pi)->dump();
					
					ValidateCallExprParams( *ai, **pi );
					
				}
				
			} else {
				// Parameter list length mismatch. Probably a varargs function. FIXME?
				// assert ( 0 && "UNSOUND: varargs function" );
				// in this case only look at argument passed and check if any of them has annotation
				// if so, emit an error
				//llvm::errs() << "! # of params:" << FDC->param_size() << " " << CE->getNumArgs() << "\n";
				
				auto ai = CE->arg_begin();
				for (; ai != CE->arg_end(); ++ai) {
					ValidateCallExprParamsAlone( *ai );					
				}
			}
		} else {
			// indirect call
			Expr * E = CE->getCallee();
			Decl * D = CE->getCalleeDecl(); 
			if ( !D ) {
				// this happens when calling an array's element
				QualType QT = ExtractArrayElementType( E );
				
				//assert ( IsFunction(QT) && "QT not a function" );
				
				ValidateIndirectCall( CE, E, QT );
					
			} else {
				
				//D->dump();
			
				// originally, I used a VarDecl here... but this can also be a FieldDecl, etc
				// ValueDecl is a parent of all of them
				ValueDecl * VD = llvm::dyn_cast_or_null<ValueDecl>(D);
				assert ( VD && "calling not a ValueDecl" );
				
				ValidateIndirectCall( CE, E, *VD );
			}
		}
		
		return true;
	}
	
	template<class T>
	void ValidateIndirectCall( CallExpr * CE, Expr * E,  T t) {
				
		if ( IsFunctionNoPrototype( t ) ) {
			Annotator.EmitNotSupportedError(CE, UNSUPPORTED_VAR_PARAM_FUNCTIONS);
			return; // return immediatly as assertion below for ExtractFunctionPrototype() will fail
		}
		
		// it's possible we're calling a uintptr_t and casting it somehow. There's no way we can catch this...
		// musl uses this, eg in __libc_start_main.c, also atexit.c and exit.c It would be easy to change the code to create a function pointer variable instead.
		if ( IsBuiltIn(t) ) { return; }
		
		// this adds the annotation for the function pointer
		llvm::StringRef VD_Anno = ExtractAnnotation( t );
		Annotator.AddAnnotation(*E, VD_Anno);	// we must annotate this E, as it contains the variable that will be "called" by the indirect call
		
		// this checks that each parameters is valid, ie with correct annotation
		FunctionProtoType * FPT = ExtractFunctionPrototype( t );
#if ISLIBC
		// TODO: emit a warning instead?
		if ( !FPT && CurrentFunction && CurrentFunction->getName() == "__dls2" && Context.getSourceManager().getFilename( E->getExprLoc() ) == "ldso/dynlink.c" ) { return; }
#endif		
		assert ( FPT && "FPT is null" );
		//FPT->dump();
		
		if ( FPT->getNumParams() == CE->getNumArgs() ) {
	
			auto pi = FPT->param_type_begin();
			auto ai = CE->arg_begin();
			
			for (; pi != FPT->param_type_end() && ai != CE->arg_end(); ++pi, ++ai) {
				//(*ai)->dump();
				//(*pi)->dump();
									
				// check each parameter
				// Note : we dont need to handle structures as parameters, since these will be flagged by the compiler
				
				ValidateCallExprParams( *ai, *pi );
			}
		} else {
			assert ( 0 && "UNSOUND: varargs function (3)" );
		}
		
	}
	
	// a = b
	bool VisitBinAssign(BinaryOperator *E) {
				
		// pFunc = &hash
		// pFunc = hash
		// s.pFunc = hash
		// s.s2.etc.pFunc = hash
		// s = s2 with possible nested structure
		
		Expr * LHS_E = E->getLHS(); assert ( LHS_E && "LHS_E is null" );
		QualType QT;
		ValueDecl *LHS_VD = ExtractDecl(LHS_E, &QT); // this does not handle weird asignments like (b ? a : c) = blabla that appears as ConditionalOperator
		if ( LHS_VD ) {	// can be 0 if, eg it's ArraySubscriptExpr. WARNING: need this to support array of function pointers? -> TODO
			
			Expr * RHS_E = E->getRHS(); assert ( RHS_E && "RHS_E is null" );
		
			if ( IsStructure( *LHS_VD ) ) {
				ValidateStructureVarDeclInitializer( LHS_VD, RHS_E ); 
			} else {
				ValidateNonStructureVarDeclInitializer( LHS_VD, RHS_E ); 
			}
			
		} else if ( QT != QualType() ) {
			
			//QT.dump();
			
			//assert ( IsFunction(QT) && "QT not a function" );
			
			// any sort of arrays really
			// arr[x] = ?? with arr containing function pointers
			// arr[x][y] = ??
			// arr[x] = (struct s){a,b,c};
			Expr * RHS_E = E->getRHS(); assert ( RHS_E && "RHS_E is null" );
		
			if ( IsStructure( *RHS_E ) ) {
				ValidateStructure(RHS_E, QT);		
			} else {
				ValidateNonStructure(RHS_E, QT);		
			}			
		}
		
		return true;
	}
	
	// =================================================================
	//
	//			TraverseXXX to tell when to stop trasversing
	//
	// =================================================================
	
	bool TraverseDecl(Decl *D) {
		
		auto *FD = dyn_cast<FunctionDecl>(D);
		if (FD) { 
			assert ( !CurrentFunction && "CurrentFunction not reset" );
			CurrentFunction = FD; 
		}
		
		// logic here
		RecursiveASTVisitor<PluginExampleVisitor>::TraverseDecl(D); // Forward to base class
		
		if (FD) { 
			assert ( CurrentFunction && CurrentFunction == FD && "CurrentFunction improperly set. Unsupported function declaration inside functions." );
			CurrentFunction = NULL; 
		}
		
		return true; // Return false to stop the AST analyzing
	}
	
	bool TraverseType(QualType T) {
		
		
		RecursiveASTVisitor<PluginExampleVisitor>::TraverseType(T);
		return true;
	}
	
	bool TraverseStmt(Stmt *S) {
		
		RecursiveASTVisitor<PluginExampleVisitor>::TraverseStmt(S);
		
		return true;
	}
	
	Stmt * GetCurrentStmt() {
		assert ( !StmtStack.empty() && "StmtStack is empty" );
		return StmtStack.top();
	}
	
	// Disable "data recursion", which skips calls to Traverse*.
	//bool shouldUseDataRecursionFor(Stmt *S) const { return false; }

private:		
	
	ASTContext & Context;
	PluginAnnotation Annotator;
	FunctionDecl * CurrentFunction = NULL;
	std::stack<Stmt *> StmtStack;
};

// DANGEROUS HACK
// This is to gain access to the frontend's consumer order so we can change it
// (below).
class HackyMultiplexConsumer : public SemaConsumer {
public:
  std::vector<ASTConsumer*> Consumers;
};


class PluginExamplerConsumer : public ASTConsumer {
public:
    explicit PluginExamplerConsumer(CompilerInstance & CI, bool _Instrument)
        : CompInst(CI), Context(CI.getASTContext()), Visitor(CI), Instrument(_Instrument) {}

    //virtual void HandleTranslationUnit(ASTContext &Context) {
		//llvm::errs() << "HandleTranslationUnit\n";
        //Visitor.TraverseDecl(Context.getTranslationUnitDecl());
    //}
    
    // this breaks down the TranslationUnitDecl into its constituents
    // It's simple to ignore the system header stuff here: that's why I
    // use it over HandleTranslationUnit
    virtual bool HandleTopLevelDecl(DeclGroupRef DG) {
		
		//llvm::errs() << "HandleTopLevelDecl\n";
		for (auto it : DG) {
			if ( ! isInSystemHeader( it ) ) {
				Visitor.TraverseDecl(it);
			}
		}
		return true;
	}
	
	// WARNING: this is the trick by Quala to be able to add annotation (ie change the type)
	virtual void Initialize(ASTContext &Context) {
		
		if (Instrument) {
			// DANGEROUS HACK
			// Change the order of the frontend's AST consumers. The
			// MultiplexConsumer contains references to (a) the "real" Clang
			// consumer, which does codegen and such, and (b) all the plugin
			// consumers. We need to move this plugin to the front of the list to
			// run before codegen. This is *certainly* unsupported and, to make it
			// even worse, requires hackery to access the private consumer vector
			// in MultiplexConsumer. Too bad.
			auto *mxc = cast<MultiplexConsumer>(&CompInst.getASTConsumer());
			auto *hmxc = (HackyMultiplexConsumer *)mxc;

			// Find this consumer in the list.
			auto it = std::find(hmxc->Consumers.begin(),
							  hmxc->Consumers.end(),
							  this);
			assert(*it == this && "consumer not found in multiplex list");

			// Move this from its current position to the first position.
			hmxc->Consumers.erase(it);
			hmxc->Consumers.insert(hmxc->Consumers.begin(), this);
		}
	}
  
private:
	bool isInSystemHeader( Decl *D ) {
		auto Loc = D->getLocation();
		if (Loc.isValid()) {
			if (Context.getSourceManager().isInSystemHeader(Loc)) {
				return true;
			}
		}
		return false;
	}
	
    CompilerInstance & CompInst;
    ASTContext & Context;
    PluginExampleVisitor Visitor;
    bool Instrument = true;
};

// Note: ASTFrontendAction is for standalone sw using clang toolong lib
// here we declare a plugin instead
class PluginExampleAction : public PluginASTAction {
protected:
	
    // this gets called by Clang when it invokes our Plugin
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef file) {
		return llvm::make_unique< PluginExamplerConsumer>(CI, true);
    }

    // implement this function if you want to parse custom cmd-line args
    bool ParseArgs(const CompilerInstance &CI, const std::vector<std::string> &args) {
        for (size_t i = 0, e = args.size(); i != e; ++i) {
			if (args[i] == "-some-arg") {
			  // Handle the command line argument.
			}
		}
		return true;
    }
};

}	// namespace

// Note: to get a pragma plugin, see http://clang.llvm.org/docs/ClangPlugins.html
static FrontendPluginRegistry::Add<PluginExampleAction>
X("fpness", "prevent null pointer dereferences");
