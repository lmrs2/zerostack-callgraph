import os, sys, traceback, errno
from optparse import OptionParser
#import glob
import re
from collections import namedtuple
import struct
import math
import dispatch
import stat
import copy

# ======================================================================
#						CONFIGURATIONS: TODO: in config file
# ======================================================================

# functions that are recursive but have a maximum number of loops
# WARNING: i'm not specifying the library for these function
# TODO: add command line option to specify this list and the associated metafile
FINITE_REC_FUNCTIONS = {"fflush":2, "getservbyport_r":2, "mwrite":2, "ms_write":2, "sw_write":2, "cgt_init2":2, "cgt_init":2}

# infeasible path
# eg: aio_cancel calls fnctl(fd,F_GETFD). fnctl calls __syscall_cp() only if arg is F_SETLKW
#INFEASIBLE_PATH = [["aio_cancel", "fcntl", "__syscall_cp"]]

# these are compiled into native, so we must inspect them manually. If defined in the module, we'll take it from module
# since we look into them first
# "fname":(stackSize, [array_regs], [array_callees])
COMPILER_RT_FUNCTIONS = {"__udivti3":(0x88,[],["__udivmodti4"]), "__udivmodti4":(0x68,["RET_RAX","RDI","RSI","RDX","RCX","R8"],[])}	# add to this list compiler's runtime compile-rt, the equivalent of libgcc

# these are libc functions written in assembly, so we don't have them in metafiles
# "fname":(stackSize, [array_regs], [array_callees])
# TODO: add command line option to specify which metafile is libc
LIBC_ASM_FUNCTION = {"memcpy":(0,[],[]), "memset":(0,[],[]), "fabs":(0,[],[]), "fmodl":(0,[],[]), "__syscall_cp_asm":(0,["RAX","R11","RDI","RSI","RDX","R10","R8","R9"],[]) }

# for this, I used the get-vdso.c to dump it. I think from https://gist.github.com/peadar/ca65a7dbac28f7e9173d8b164c0c4fc9
VDSO_FUNCTION = {"__vdso_time":(8,["RET_RAX"],[])}	# TODO: __vdso_clock_gettime, clock_gettime/gettimeofday/time (WHATSNEW)

# the list of intrinsic functions http://llvm.org/docs/LangRef.html
# all intrinsics are lowered to builtins (inline) or to calls. We conservatively lower them
# to calls in our code, so need not redefined them
#LLVM_INTRINSICS_FUNCTIONS = { "llvm.memset.p0i8.i64":0, "llvm.memcpy.p0i8.p0i8.i64":0 }
LLVM_INTRINSICS_TO_IGNORE = {"llvm.lifetime.start", "llvm.lifetime.end", "llvm.va_end", "llvm.va_start", "llvm.va_copy"}

# WARNING: this is just a hack before I code the qualified type thing for function pointers
#IGNORE_FUNCTION_POINTER = ["__shgetc", "vfprintf", "rsa_private", "rsa_pkcs1_decrypt", "__fflush_unlocked", "__fwritex", "__overflow", "main", "__uflow", "__toread"]

# functions that are replaced by LLVM automatically based
X64_AUTO_REPLACE_FUNCTIONS = ["tmpfile64", "fopen64", "freopen64", "fseeko64", "ftello64", "fgetpos64", "fsetpos64"]


# for Intel's x86_64's redzone
RED_ZONE = 128

# for signal delivery, cpu state is stored on process's stack
# // signal/interrupts with process CPU state saved on user stack http://users.cms.caltech.edu/~donnie/cs124/CS124Lec17.pdf
# do_signal() -> get_signal()
#			   -> handle_signal() -> setup_rt_frame() -> *_setup_rt_frame() which saves cpu state
# 
# TODO - probably pass as argument rather than hard-coded

# ======================================================================
#		constants: DO NOT CHANGE
# ======================================================================

# number of nops inserted by backend pass
_NB_EMPTY_INST = 20
_NOOP_SIZE = 3	# for x86
NOOP_SPACE = _NB_EMPTY_INST * _NOOP_SIZE

# name for function pointer which is not annotated
UNKNOWN_FUNCTION_POINTER = "@UNKNOWN_POINTER"
ANNOTATED_FUNCTION_POINTER = "@ANNOTATED_POINTER"
BUILTIN_SYSCALL = "@INSTRUCTION_SYSCALL"
SENSITIVE_FUNCTION = "@SENSITIVE"
ANNOTATED_FUNCTION = "@type_annotate"

# keys for the hashmap
# libc's C code
LIBC_C_KEY = "__LIBC_C_FUNCTIONS__"
# libc's handwritten asm functions
LIBC_ASM_KEY = "__LIBC_ASM_FUNCTIONS__"
# LLVM intrinsics key
#LLVM_INTRINSICS_KEY = "__LLVM_INTRINSICS_FUNCTIONS__"
# VDSO key
VDSO_KEY = "__VDSO_FUNCTIONS__"
# keys for compiler runtime
COMPILER_RT_KEY = "__COMPILER_RT_FUNCTIONS__"
# for instruction functions, ie syscall for now
BUILTIN_KEY = "__BUILTIN_INST_FUNCTIONS__"
# for annotated functions
ANNOTATED_KEY = "__ANNOTATED_FUNCTIONS__"


def vdsoname_to_annotatedname(vdsoname):
	MUSL_TAG = "tag_musl_"
	return MUSL_TAG + vdsoname;

def foo_callback(option, opt, value, parser):
	setattr(parser.values, option.dest, value.replace(' ','').split(','))
  
def get_options(parser):
	
	# http://docs.python.org/2/library/optparse.html
	#usage = "usage: %prog [options] arg"
	#parser = OptionParser(usage)
	
	#parser.add_option('-n', "--nsamples", action="store", dest="nsamples", type="int", help='number of samples in FILENAME')
	
	parser.add_option("--inobject", 
					#type='choice',
					action='store',
					dest='inobject',
					default = None,
					help='object to patch. Can be a .o/.so/executable file. It just needs to be binary :)')
	
	parser.add_option("--outobject", 
					#type='choice',
					action='store',
					dest='outobject',
					default = None,
					help='output object file')
	
	parser.add_option('--inmetafiles',
                  type='string',
                  action='callback',
                  callback=foo_callback,
                  help='list of files containing stack/reg info to compute the size of stack to erase')
                  
	parser.add_option("--platform", 
					type="choice",
					choices=["x86_32", "x86_64"],
					help='platform we\'re compiling for')
					
	parser.add_option("--libc", 
					type="choice",
					choices=["musl"],
					help='libc used')
	
	parser.add_option("--bulk-register-zeroing",
					action="store_true",
					dest="brz",
					help='Bulk register zeroing')
					
	parser.add_option("--signal-stack-use",
					type='int',
					action='store',
					dest='ssu',
					default = None,
					help='stack size used to store CPU state in signal handler (bytes)')
					             				
	parser.add_option("-v", "--verbose",
					action="store_true", 
					dest="verbose")
	
    
	return parser.parse_args()


def check_options(parser, options, args):
	
	if options.inobject == None:
		parser.error("INOBJECT not supplied")
			
	if options.outobject == None:
		parser.error("OUTOBJECT not supplied")
		
	if options.inmetafiles == None:
		parser.error("INMETAFILES not supplied")
	
	if options.platform == None:
		parser.error("PLATFORM not supplied")
		
	if options.ssu == None:
		parser.error("SIGNAL-STACK-USE not supplied")
		
	if options.libc == None:
		parser.error("LIBC not supplied")
		
def silentremove(filename):
	try:
		os.remove(filename)
	except OSError as e: # this would be "except OSError, e:" before Python 2.6
		if e.errno != errno.ENOENT: # errno.ENOENT = no such file or directory
			raise # re-raise exception if a different error occured


def isLLVMIntrinsicsToIgnore( fname ):
	return (fname in LLVM_INTRINSICS_TO_IGNORE)

def isLLVMIntrinsics( fname ):
	return ( fname[:5] == "llvm." )

def isCompilerRT( fname ):
	return ( fname in COMPILER_RT_FUNCTIONS )
	
def isCompilerRT( fname ):
	return ( fname in COMPILER_RT_FUNCTIONS )

def formatCalleeForPathStr( metafile, callee ):
	return os.path.basename(metafile) + ":" + callee

def isFiniteRecursive( fname ):
	return (fname in FINITE_REC_FUNCTIONS)

def isFiniteAnnotatedRecursive( fname ):
	return (fname in FINITE_REC_ANNO_FUNCTIONS)


def getUndecoratedLLVMIntrinsic( llvmfName ):
	
	match = re.match(r'^llvm[.]([a-zA-Z]*)[.](.*)$', llvmfName)
	assert ( match != None and "Cannot undecorate function name" )
	return match.group(1)


def formatForAnnotatedKey( annotation ):
	return ANNOTATED_KEY + annotation


# functionInPath = string of[ f1:f2:f3: ]
def walkTree(tree, metafile, libc, fname, functionInPath):
		
	# WARNING: we must be able to detect cycles:TODO
	# currently I have hard-coded musl-libc functions
	# but in the end we'll want to let the compilation go and check here
	# if the user has declared a function sensitive where there is a cycle.
	# Once the cycle detection is implemented
	
	print "walkTree:", formatCalleeForPathStr( metafile, fname )
	
	#if fname == "tag_musl_pthread_cancelbuf":
		#print tree[ANNOTATED_KEY]["tag_musl_pthread_cancelbuf"]
		#assert(0)
	#if fname == "__syscall_cp":
		#for k in tree:
			#if "__syscall_cp" in tree[k]:
				#print tree[k]["__syscall_cp"]
				#assert(0)
	
	plainFunctionList = set( [lv for k,v in tree.iteritems() for lv in v ] )
	if ( fname not in plainFunctionList ):
		msg = "Function '" + fname + "' cannot be found. Have you passed all metafiles?"	
		raise Exception(msg)
		
	t = tree[metafile][fname]
	functionStack = t[0]
	functionRegs = t[1]
	
	
	#============
	# handle special case of functions that are recursive BUT can call
	# themselves only a limited number of times
	#if fname in SUPPORTED_REC_FUNCTIONS:
	#	# the source code shows there's only one possible nested call, so we use twice the stack usage
	#	functionStack *= supportedRecFunctions[fname]
	#	print fname, supportedRecFunctions[fname]
	#	msg = "We got call to '" + fname + "' which can call itself only " + str(supportedRecFunctions[fname]) + " times. Remove exception." 
	#	raise Exception(msg)
	# ===========
	
	assert ( functionStack >= 0 )
	functionMaxPath = ''
	functionMaxRegisters = functionRegs
	functionMaxStack = functionStack
	
	calleeMax = 0
	calleeMaxFname = ''
	calleeMaxPath = ''
	calleeRegList = []
	orgTree = None
	
	for callee in t[2]:		
		
		# orgTree keeps the original tree, ie before we've removed some entries for recursive functions thru annotated pointers
		if orgTree != None:
			tree = orgTree
			orgTree = None
			
		# check here if this call a pointer to a function without annotation
		if callee == UNKNOWN_FUNCTION_POINTER:
			# HACK for now. Let it continue f this is libc f->read() call from __shgetc
			# after I will annotate the functions
			
			#if fname in IGNORE_FUNCTION_POINTER:
			#	continue
			funcList = StringifyFunctionList(functionInPath, " -> ", "\n\t")
			msg = "Function '" + fname + "' calls a function pointer without annotation\nPATH='" + funcList + "'"
			raise Exception(msg)
		
		
		# check if this is AIO, which we dont support coz it leads to cycles
		# __aio_close() checks counter aio_fd_cnt before it calls aio_cancel(). No AIO calls means this counter is 0
		# I also check for __stdio_close because __aio_close() gets inline into __stdio_close()
		if (fname == "__aio_close" and callee == "aio_cancel" ) or (fname == "__stdio_close" and callee == "aio_cancel"):
			print " Skipping infeasible AIO path:", fname, "->", callee
			continue
			
		## check if the path is infeasible in practice
		## REMOVE from now since the AIO test takes care of it
		#for path in INFEASIBLE_PATH:
			
			#if fname == path[0] and callee == path[1]: 
			
				#assert ( orgTree == None )
				#orgTree = tree
				#tree = copy.deepcopy(tree)
				#found = False
				#func2remove = path[2]
				#for k in tree:
					#if callee in tree[k]:
						#assert ( func2remove in tree[k][callee][2] )
						#print tree[k][callee][2]
						#tree[k][callee][2].remove(func2remove)
						#print tree[k][callee][2]
						#found = True
				#assert (found == True)
				#print " Skipping infeasible path:", fname, "->", callee, "->", func2remove
				#break	# we're done
			
		
		metafileKey = ""
		
		if isLLVMIntrinsics( callee ):
			
			if isLLVMIntrinsicsToIgnore( callee ):
				print " Skipping intrinsics", callee
				continue	
			
			# Talk to David on 30 Sept. Intrinsics may be lowered to inline stuff or
			# replaced by a call. The call could be to a function in the current module, 
			# or in libc ... or even womewhere else? 
			# I assume there's only one definition in the whole project/executable.
			# so I first check that, then
			# I use the unique module where the function is declared
			
			# get the name of this function without the LLVM decoration
			callee = getUndecoratedLLVMIntrinsic( callee )
			defFuncs = [k for k,v in tree.iteritems() for lv in v if lv == callee ]
			msg = ""
			if len(defFuncs) == 0:
				msg = "Function '" + callee + "' called by '" + formatCalleeForPathStr( metafile, fname ) + "' cannot be resolved (not found)"
			elif len(defFuncs) > 1:
				sep = "\n\t- "
				msg = "Function '" + callee + "' called by " + formatCalleeForPathStr( metafile, fname ) + " is defined in multiple metafiles:" + sep + sep.join(defFuncs)
			
			if len(msg):
				raise Exception(msg)
			
			# here we're sure there's only one definition of this function throughout the executable
			#metafileKey = LLVM_INTRINSICS_KEY
			metafileKey = defFuncs[0]
		
		# check if this is a call to an annotated function pointer
		elif callee[:len(ANNOTATED_FUNCTION_POINTER)] == ANNOTATED_FUNCTION_POINTER:
			annotation = callee[len(ANNOTATED_FUNCTION_POINTER)+1:]	# account for the extra _
			assert ( len(annotation) );
			metafileKey = ANNOTATED_KEY #formatForAnnotatedKey( annotation )
			#print metafileKey
			#print tree[ metafileKey ]
			if  metafileKey not in tree:
				msg = "Cannot find function annotated with '" + annotation + "'"
				raise Exception( msg )
			callee = annotation
			#assert (0)
		elif callee in tree[metafile]:
			metafileKey = metafile
		elif isCompilerRT( callee ):
			metafileKey = COMPILER_RT_KEY	
		else:
			# we're left trying to resolve the symbol
			# let' sanity check that the symbol we're looking for appears only once, 
			# otherwise we're doomed :) .. we'll have to look for weak vs strong symbols, etc
			# This is probably out of scope
			# at this point we already know the function is not defined in compiler-rt, llvm intrinsics, and the current module.
			# so whatever allKeys holds, it has to be from other modules
			allKeys = set( [k for k,v in tree.iteritems() for lv in v if lv == callee] ) # i assume the same function is defined only once in each module
			msg = ""
			
			# we could not find the function. Try one last time to see if this is a function replaced by LLVM that does not exist in msul-libc
			# LLVM automatically replaces, eg fopen() with fopen64() if we're compiling for x86_64
			if len(allKeys) == 0 and callee in X64_AUTO_REPLACE_FUNCTIONS and libc == "musl":
				callee = callee[:len(callee)-2]	# remove the tailing '64'
				allKeys = set( [k for k,v in tree.iteritems() for lv in v if lv == callee] )
			
			# have we found the function?	
			if len(allKeys) == 0:					
				msg = "Function '" + callee + "' called by '" + formatCalleeForPathStr( metafile, fname ) + "' cannot be resolved (not found 2)"
			elif  len(allKeys) > 1:
				sep = "\n\t- "
				msg = "Function '" + callee + "' called by " + formatCalleeForPathStr( metafile, fname ) + " is defined in multiple metafiles:" + sep + sep.join(allKeys)
			if len(msg) > 0:
				raise Exception(msg)
			
			# we found the function definition
			metafileKey = list(allKeys)[0]
		
		calleeStrForPath = formatCalleeForPathStr(metafileKey, callee)
		
				
		# now that we've resolved the callee, check for cycles in CFG
		if calleeStrForPath in functionInPath:
			
			# function that calls itself
			if callee == fname and isFiniteRecursive( fname ):
				# function calls itself. We cannot just check:
				#	 isFiniteRecursive( fname ) because we would miss other callees
				#	 isFiniteRecursive( callee ) because we could miss the callee entirely when called from another function
				# we're sure this is the function care about because there is no duplicate definitions
				print " Skipping finite recursive callee", callee
				continue
			
			# function that calls itself thru an (annotated) function pointer
			# eg meta_libc.machine:cgt_init -> __ANNOTATED_FUNCTIONS__:tag_musl___vdso_time
			elif isFiniteRecursiveThruAnnotatedPointer(tree, metafile, fname, metafileKey, callee):
				print " Found finite recursive callee thru annotated function pointer", callee, "from", fname
				assert ( orgTree == None )
				orgTree = tree
				tree = copy.deepcopy(tree)
				assert ( fname in tree[ANNOTATED_KEY][callee][2] )
				#print callee, tree[ANNOTATED_KEY][callee][2]
				#print "REMOVED (1)", fname, "FROM", callee
				tree[ANNOTATED_KEY][callee][2].remove(fname)
				#print callee, tree[ANNOTATED_KEY][callee][2]
			
			else:
				#funcList = ""
				#for f in functionInPath:
				#	funcList += f + ' -> '
				print "fname:", fname
				print "callee:", callee
				print "metafileKey:", metafileKey
				print "calleeStrForPath:", calleeStrForPath
				#print "callee is annotation:", calleeIsAnno
				#print "fname annotation:", functionAnno
				print "callee:", callee
				print ""
				funcList = StringifyFunctionList(functionInPath, " -> ", "\n\t")
				msg = "Found a loop: " + funcList + " -> " + calleeStrForPath
				msg = msg.replace(callee, bcolors.YELLOW + callee + bcolors.ENDC)
				raise Exception(msg)
		
		# get the path/stack for this function	
		localPath, localMax, localRegList = walkTree(tree, metafileKey, libc, callee, functionInPath + [calleeStrForPath])
		
		# always update the register list
		calleeRegList = localRegList
		
		# update the max stack usage if we've found a greater one
		if calleeMax < localMax:
			calleeMax = localMax
			calleeMaxFname = callee
			calleeMaxPath = localPath
			#calleeRegList = localRegList
	
	# WARNING: this is important:
	# "remove" the RET_* registers from callees, in the sense that they are no longer returned registers (ie remove the RET).
	# do NOT remove them blindly, since we MUST erase them (eg RET_XMM0 returned frm a callee MUST appear in list of registers to erase as XMM0)
	# even if callee are sensitive, they are not a top-level function
	# this means only functions at the top (which are not callees), will keep the RET_* registers
	# the RET_ register will NOT be erased, but its top bits will
	calleeRegList = [v.replace("RET_","") for v in calleeRegList if len(v) and v != "RET_VOID"]
	functionMaxRegisters = [v for v in list(set( functionMaxRegisters + calleeRegList )) if len(v)]
	functionMaxStack = functionStack + calleeMax
	#print fname, "calleeMax:", calleeMax
	#print fname, "functionStack:", functionStack
	
	isRecursive = isFiniteRecursive( fname )
	
	# now format the path
	functionMaxPath = bcolors.TURQUOISE + os.path.basename(metafile) + bcolors.ENDC + ":" + bcolors.BLUE + fname + bcolors.ENDC + '(' + bcolors.GREEN + str(functionStack) + bcolors.ENDC + '/' + bcolors.GREEN + str(functionMaxStack) + bcolors.ENDC + ')'
	# add the number of maximum recursions if need be
	if isRecursive:
		functionMaxPath = functionMaxPath + bcolors.FAIL + "*" + str(FINITE_REC_FUNCTIONS[fname]) +  bcolors.ENDC
	# add the path of the callee if there's one
	if calleeMaxPath != '':
		functionMaxPath = functionMaxPath + ' -> ' + calleeMaxPath
	#if calleeMaxPath == '':
		#functionMaxPath = bcolors.TURQUOISE + os.path.basename(metafile) + bcolors.ENDC + ":" + bcolors.BLUE + fname + bcolors.ENDC + '(' + bcolors.GREEN + str(functionStack) + bcolors.ENDC + '/' + bcolors.GREEN + str(functionMaxStack) + bcolors.ENDC + ')'
	#else:
		#functionMaxPath = bcolors.TURQUOISE + os.path.basename(metafile) + bcolors.ENDC + ":" + bcolors.BLUE + fname + bcolors.ENDC + '(' + bcolors.GREEN + str(functionStack) + bcolors.ENDC + '/' + bcolors.GREEN + str(functionMaxStack) + bcolors.ENDC + ')' + ' -> ' + calleeMaxPath
	
	
	# if this is a finite recursive function, multiply the stack usage accordingly
	if isRecursive:
		print "mutiplying functionMaxStack", functionMaxStack, "for", fname, "by", FINITE_REC_FUNCTIONS[fname]
		functionMaxStack *= FINITE_REC_FUNCTIONS[fname]
	
	#assert (fname != "cgt_init")
	return functionMaxPath, functionMaxStack, functionMaxRegisters

# to find out the annotation of a function, we check if it's one of the callees
# of a function under ANNOTATED_KEY
# WARNING: this can give false positive because it does not take into account the key part...
# unfortunately this requires reshuffling the code
def AnnotationOf( tree, key, fname ):
	if IsAnnotation(key, fname):
		return ""
		
	for k in tree[ANNOTATED_KEY]:
		listCallees = tree[ANNOTATED_KEY][k][2]
		if fname in listCallees:
			return k
	return ""
	
	
def IsAnnotation( key, fname ):
	return key == ANNOTATED_KEY

def isFiniteRecursiveThruAnnotatedPointer(tree, metaf, fname, metac, callee):
	
	# try to bail out as soon as we cann
	#if not isFiniteAnnotatedRecursive( fname ):
	if not isFiniteRecursive( fname ):
		return False
		
	calleeIsAnno = IsAnnotation(metac,callee)
	functionAnno = AnnotationOf( tree, metaf, fname )
	
	return (calleeIsAnno and len(functionAnno)>0 and functionAnno==callee ) 
	
		
# extra is some decoration to write between functions
def StringifyFunctionList(fList, calls, extra):
	funcList = ""
	for f in fList:
		funcList += f + extra + calls
	return funcList[:len(funcList) - len(calls)]
	
class Dispatcher:

	def __init__(self, executablePath):
		self.executable = dispatch.read_executable(executablePath)
		# Invoke the analyzer to find functions
		self.executable.analyze()
		# Prepare the executable for code injection
		self.executable.prepare_for_injection()
		self.patched = False
		# TODO: in another class
		self.EQUIVALENCE = {0:"A", 1:"C", 2:"D", 3:"B", 6:"S", 7:"D"}
		self.SUPPORTED_REGISTERS = [
									"RAX","EAX","AX","AH","AL","R0","R0D","R0W","R0L",
									"RBX","EBX","BX","BH","BL","R3","R3D","R3W","R3L",
									"RCX","ECX","CX","CH","CL","R1","R1D","R1W","R1L",
									"RDX","EDX","DX","DH","DL","R2","R2D","R2W","R2L",
									"RSI","ESI","SI","R6","R6D","R6W",	# those don't have an H/L 8 bit
									"RDI","EDI","DI","R7","R7D","R7W",	# those don't have an H/L 8 bit
									"R8","R8D","R8W","R8L",				# frmo here on, there is only a L 8 bit
									"R9","R9D","R9W","R9L",
									"R10","R10D","R10W","R10L",
									"R11","R11D","R11W","R11L",
									"R12","R12D","R12W","R12L",
									"R13","R13D","R13W","R13L",
									"R14","R14D","R14W","R14L",
									"R15","R15D","R15W","R15L",
									"ZMM0","YMM0","XMM0",
									"ZMM1","YMM1","XMM1",
									"ZMM2","YMM2","XMM2",
									"ZMM3","YMM3","XMM3",
									"ZMM4","YMM4","XMM4",
									"ZMM5","YMM5","XMM5",
									"ZMM6","YMM6","XMM6",
									"ZMM7","YMM7","XMM7",
									"ZMM8","YMM8","XMM8",
									"ZMM9","YMM9","XMM9"
									]
	
	def patchFunctionWith( self, functionName, maxStack, maxRegs, platform ):
		"""
		4005db:	48 31 c0             	xor    %rax,%rax
		4005de:	b9 67 45 23 01       	mov    $0x1234567,%ecx				b9 ff ff ff ff for 0xffffffff
		4005e3:	48 8d bc 24 f0 cd ab 	lea    -0x76543210(%rsp),%rdi		
		4005ea:	89 
		4005eb:	f3 48 ab             	rep stos %rax,%es:(%rdi)

		4005e3:	48 8d bc 24 11 32 54 	lea    0x76543211(%rsp),%rdi		0x76543211
		4005ea:	76
		"""
		#import binascii
		
		# get the raw version for patching
		patchMOV, patchLEA = self.rawOffsetsFromStack( maxStack, platform )
		
		THE_RET_INS = "\xc3"
		THE_MOV_INS = "\xb9\x67\x45\x23\x01"
		THE_LEA_INS = "\x48\x8d\xbc\x24\xf0\xcd\xab\x89"
		THE_MOV_PATCH = "\xb9" + patchMOV
		THE_LEA_PATCH = "\x48\x8d\xbc\x24" + patchLEA
		LEA_insertion_pos = 0
		MOV_insertion_pos = 0
		# in the LLVM pass, we support only a single RET for sensitive function
		RET_pos = 0
		
		for function in self.executable.iter_functions():
			if function.name == functionName:
				for i in range(len(function.instructions)):
					instruction = function.instructions[i]
					#print instruction
					# for finding the instruction, we can:
					# 1. compare their string representation
					# 2. compare the mnemonics and their operands' string representation -- coz there is not API to inspect the instruction it seems
					# 3. compare the raw bytes
					if instruction.raw == THE_MOV_INS:
						#print "we found the MOV", i, "0x" + binascii.hexlify(instruction.raw), "at address", instruction.address
						assert ( 0 == MOV_insertion_pos )	# this will catch sensitive functions with multiple returns -  we can support them but need to ensure we're doing this properly
						MOV_insertion_pos = instruction.address
						#self.executable.replace_instruction(instruction.address, patchMOV)
						
						assert ( i < len(function.instructions) )
						instruction = function.instructions[i+1]
						#print "next instruction: '0x" + binascii.hexlify(instruction.raw) + "'"
						assert ( instruction.raw == THE_LEA_INS )
						assert ( 0 == LEA_insertion_pos )	# this will catch sensitive functions with multiple returns
						LEA_insertion_pos = instruction.address
						#self.executable.replace_instruction(instruction.address, patchLEA)
					
					elif instruction.raw == THE_RET_INS:
						assert ( 0 == RET_pos )	# catch multiple RET instruction in sensitive functions
						RET_pos = instruction.address
		
		if not (LEA_insertion_pos !=0 and MOV_insertion_pos != 0) :
			msg = "Could not locate the code to zero the stack in function '" + functionName + "'"
			raise Exception( msg )
		
		# patch now for the stack. Not sure it's feasible to patch above because it may screw up the loop
		# WARNING: instruction replacement must be the same size, coz the lib cannot patch all the address jmp etc
		self.executable.replace_at(MOV_insertion_pos, THE_MOV_PATCH)
		self.executable.replace_at(LEA_insertion_pos, THE_LEA_PATCH)
		
		if len(maxRegs) and RET_pos == 0:
			msg = "Could not locate the code to zero registers in function '" + functionName + "'"
			raise Exception( msg )
		
		# patch now for registers
		self.zeroRegisters(maxRegs, RET_pos)
		
		
		#print "patch", binascii.hexlify(THE_MOV_INS), "with", binascii.hexlify(THE_MOV_PATCH)
		#print "patch", binascii.hexlify(THE_LEA_INS), "with", binascii.hexlify(THE_LEA_PATCH)
		
		self.patched = True
	
	def CleanupRegisters(self, regList):
		cleanRegList = []		
		dirtyRegList = [e for e in regList if e[:4]!= "RET_"]
		for Reg in dirtyRegList:
			if not self.isSubsetFromList([e for e in dirtyRegList if e != Reg], Reg): 
				cleanRegList += [Reg]
		return cleanRegList
	
	def zeroRegisters(self, regList, RetPos):
		# WARNING: instruction replacement must be the same size, coz the lib cannot patch all the address jmp etc
		# That's why the LLVM backend pass has prepared 3 noops for each xor instruction, with 10 instructions max
		# so we must check we have at enough space:TODO
		insertionPos = RetPos - NOOP_SPACE
		
		# I remove check below because some registers may be the same, and also because we must zero returned values
		# all instruction are not 3 bytes. SO instead i check just before replacing instructions
		#if ( len(regList) > (NB_NOPS/NOP_SIZE) ):
		#	msg = "Too many registers to zero out (" + ",".join(regList) + ")"
		#	raise Exception( msg )
		
		# remove RSP itf it's present
		if "RSP" in regList:
			regList.remove("RSP")
		
		# save returned registers
		RET_Regs = [v[4:] for v in regList if v[:4] == "RET_"]
		
		# before we move on, shrink the regList to its minimum size, ie remove those registers that are subset of others
		# this wil discard the returned registers, that's why we saved them before
		regList = self.CleanupRegisters(regList)
		
		# printing here is ugly, for the moment it just helps debugging...
		print 'regList:' + bcolors.GREEN + ",".join(regList) + bcolors.ENDC
		
		# Note: of course there's a way to encode those... but I start with this simple solution for now
		# https://defuse.ca/online-x86-assembler.htm#disassembly
		XOR_ARRAY = {	
						# 128bit registers
						"XMM0"	:	"\x0F\x57\xC0",	# xorps
						"XMM1"	:	"\x0F\x57\xC9",
						"XMM2"	:	"\x0F\x57\xD2",
						"XMM3"	:	"\x0F\x57\xDB",
						"XMM4"	:	"\x0F\x57\xE4",
						"XMM5"	:	"\x0F\x57\xED",
						
						# 64bit registers
						"RAX"	:	"\x48\x31\xC0",
						"RCX"	:	"\x48\x31\xC9",
						"RDX"	:	"\x48\x31\xD2",
						"RDI"	:	"\x48\x31\xFF",
						"RSI"	:	"\x48\x31\xF6",
						"R8"	:	"\x4D\x31\xC0",
						"R9"	:	"\x4D\x31\xC9",
						"R10"	:	"\x4D\x31\xD2",
						"R11"	:	"\x4D\x31\xDB",
						
						# 32bit registers
						"EAX"	:	"\x31\xC0",
						"EDX"	:	"\x31\xD2",
						"ESI"	:	"\x31\xF6",
						
						"R9D"	:	"\x45\x31\xC9",
						"R10D"	:	"\x45\x31\xD2",
						# 16bit register
						
						# 8bit registers
					}
		
		ZERO_RET_ARRAY = {
						# mov    eax,eax. automatically zero extend 32bit to 64bit registers
						"EAX"	:	"\x89\xC0"
						# WARNING: for other registers smaller than 32 bits, we must use movzx instead
						# eg movzx rax, ax -> "\x48\x0F\xB7\xC0"
						}
		
		"""
		WARNING: the RET_* register is the actual register returned by the function.
			For example:
				RAX = <data>
				RET EAX
			-> 	this would appear as RET_EAX. In this case, the upper bits of RAX must be zeroed out
				
				EAX = <data>
				RET RAX
			->	this would appear as RET_RAX. In this case, we must not zero out anything.
			
			Now, consider what happens in the above example if the REG = <data> is done 
			in a callee. In the first case, we MUST 
			zero out the upper bit of RAX anyway NOW.
			In the second case, we MUST do nothing.
			
			EAX = <data>	-> EAX
			callee -> RAX = <data2>	-> RAX
			RET EX	-> RET_EX
			-> we MUST zero upper bits of EX
			
				
		"""

		REG_List = [v for v in regList if v[:4] != "RET_"]
		for regKey in REG_List:
			
			# is the register returned?
			if regKey in RET_Regs:
				# don't erase returned registers.
				continue
			
			# is the register a subset of a returned register?
			# if so, do NOT erase this register.
			if self.isSubsetFromList(RET_Regs, regKey):
				continue
				
			# is the register a superset of a returned register?
			# if so, do NOT erase this register now.
			# We will emit a "movz ret, R" at the end for ALL ret register, so it will erase any superset
			if self.isSupersetFromList(RET_Regs, regKey):
				continue
			
			#print regKey
			assert ( regKey in XOR_ARRAY )
			#print RetPos, len(XOR_ARRAY[regKey]), RetPos - len(XOR_ARRAY[regKey]), insertionPos
			assert ( RetPos - len(XOR_ARRAY[regKey]) >= insertionPos )	# make sure we have enough nop space to overwrite
			self.executable.replace_at(insertionPos, XOR_ARRAY[regKey])
			insertionPos += 3
			
		# now, emit a movz ret, bigger_ret for each returned register, to zero out upper bits
		for retReg in RET_Regs:
			# this will abort if we get RAX returned, which is very possible
			# it will also abort for any non-normalized registers, eg R[0-7][DWL]{0,1}
			# we just need to normalize the RET_Regs list first. I've not done it coz
			# I want to see if this happens in practice
			assert ( retReg in ZERO_RET_ARRAY )
			assert ( RetPos - len(ZERO_RET_ARRAY[retReg]) >= insertionPos )	# make sure we have enough nop space to overwrite
			self.executable.replace_at(insertionPos, ZERO_RET_ARRAY[retReg])
			insertionPos += 3
		
	
	# ==============================
	# util functions for registers
	# ==============================
	def isRegChild( self, T, CmpReg ):
		#print "isRegChild", T, CmpReg
			
		for k, children in T.iteritems():
			#print "compare key", k, "and", CmpReg
			if k == CmpReg:
				#print "Found (1)", CmpReg, "Children:", children
				return True
			elif self.isRegChild(T[k], CmpReg):
				return True
				
		return False				
	
	def setRegLetter(self, T, regLetter):
		origT = T
		newT = T.copy()
		for key in origT:
			origValue = origT[key]
			newValue = self.setRegLetter( origT[key], regLetter )
			newT[key.replace("_", regLetter)] = newValue
			del newT[key]
		return newT
	
	def getStartT(self, T, keyName):
		for k, elts in T.iteritems():
			if k == keyName:
				return T[k]
			else:
				t = self.getStartT(T[k], keyName)
				if t != None:
					return t
		return None		
	
	def isSubsetFromList(self, SrcRegList, CmpReg):
		#print SrcRegList
		for srcReg in SrcRegList:
			if self.isSubsetOf(srcReg, CmpReg):
				return True
		return False
		
	def isSupersetFromList(self, SrcRegList, CmpReg):
		#print SrcRegList
		for srcReg in SrcRegList:
			if self.isSupersetOf(srcReg, CmpReg):
				return True
		return False
	
	def isSupersetOf(self, SrcReg, CmpReg):
		return self.isSubsetOf(CmpReg, SrcReg)
	
	def NormalizeReg(self, Reg):
		
		# check these are registers we know about
		assert ( Reg in self.SUPPORTED_REGISTERS )
		
		# https://en.wikibooks.org/wiki/X86_Assembly/X86_Architecture 
		# register R[0-7][WDL] correpond to RAX to RDI
		
		match = re.match(r'^R([0-7])([DLW]{0,1})$', Reg)
		if match != None:
			num = int(match.group(1))
			s = match.group(2)
			regLetter = self.EQUIVALENCE[num]
			if num >= 6 and num <= 7:
				endChar = "I"
			elif num >= 0 and num <= 3:
				endChar = "X"
			elif num == 4 or num == 5:
				assert (0 and "Unsupported [4,5] for register equivalence")
			
			if s == "":
				Reg = "R" + regLetter + endChar
			elif s == "D":
				Reg = "E" + regLetter + endChar
			elif s == "W":
				Reg = regLetter + endChar
			else:
				# R6=RSI and R7=RDI. But those don't reallay have 8-lower bit equivalent it seems
				# https://www.cs.virginia.edu/~evans/cs216/guides/x86.html
				# so check that indeed, we don't have that!
				assert ( num != 6 and num != 7 and "No equivalence for R6/R7 lower 8 bits" )
				Reg = regLetter + "L"
		
		return Reg
	
	# see https://en.wikibooks.org/wiki/X86_Assembly/X86_Architecture
	def getNormalizedRegisterTree( self, Reg ):
		
		T1 = {"R_X":{"E_X":{"_X": {"_L":{}, "_H":{}}}}}
		T2 = {"R_I":{"E_I":{"_I": {}}}}
		T3 = {"R_":{"R_D":{"R_W": {"R_L":{}}}}}
		T4 = {"ZMM_":{"YMM_":{"XMM_":{}}}}
		
		# normalize the register
		Reg = self.NormalizeReg( Reg )
		
		# The regexp could be simpler, but I've decide to split cases rather than have a one size fit all formula
		if len(Reg) == 2:
			# those ending with 2 letters as AX, AL, AH, SI, etc
			match = re.match(r'^([ABCD])([XHL])$', Reg)
			if match != None:
				t = T1
				regID = match.group(1)
			
			# those with 2 letters as R[8-9] - R[0-7] should have been normalized
			if match == None:
				match = re.match(r'^R([8-9])$', Reg)
				if match != None:
					t = T3
					regID = match.group(1)
						
		elif len(Reg) == 3:
			# those ending with X, eg RAX, EAX
			match = re.match(r'^([ER])([ABCD])X$', Reg)
			if match != None:
				t = T1
				regID = match.group(2)
			# those ending with I, eg RSI, ESI
			if match == None:
				match = re.match(r'^([ER])([SD])I$', Reg)
				if match != None:
					t = T2
					regID = match.group(2)
			# R10-R15
			if match == None:
				match = re.match(r'^R(1[0-5]{1})$', Reg)
				if match != None:
					t = T3
					regID = match.group(1)
			
			# R[8-9][WDL] - 0-7 should have been normalized
			if match == None:
				match = re.match(r'^R([8-9])([WDL])$', Reg)
				if match != None:
					num = int(match.group(1))
					t = T3
					regID = match.group(1)
			
		elif len(Reg) == 4:
			# R10[WLD] - R15[WLD]
			match = re.match(r'^R(1[0-5]{1})([WLD])$', Reg)
			if match != None:
				t = T3
				regID = match.group(1)
			
			# https://en.wikipedia.org/wiki/Advanced_Vector_Extensions
			# for now only [ZXY]MM[0-1]
			if match == None:
				match = re.match(r'^([ZYX])MM([0-9])$', Reg)
				if match != None:
					t = T4
					regID = match.group(2)
			
		else:
			msg = "Unsupported registers " + Reg
			raise Exception(msg)
			
		assert ( match != None )
		#print self.setRegLetter(t, regID)
		T = self.getStartT( self.setRegLetter(t, regID), Reg )
		return T
				
	# check is CmpReg is contained in SrcReg
	# eg CmpReg = EAX and SrcReg = RAX, would return True
	def isSubsetOf(self, SrcReg, CmpReg):
		assert ( len(SrcReg)>0 and len(CmpReg)>0 )
		#print SrcReg, CmpReg
		
		tree = self.getNormalizedRegisterTree(SrcReg )
		return self.isRegChild(tree, self.NormalizeReg(CmpReg)) 
	
			
	# ==============================
	# end util functions for registers
	# ==============================
	
	def rawOffsetsFromStack( self, maxStack, platform ):
	
		# WARNING: what else must be changed for 32 bits?
		if platform == "x86_32":
			raise Exception("Platform not supported yet")
		elif platform == "x86_64":
			size = 8.
			formatOffsetSize = self.executable.pack_endianness + "l" # integer, LE '<'
			formatOffset = self.executable.pack_endianness + "l" # integer, LE '<'
		
		# WARNING: we use the ceil to make sure everything is erased
		offset_size = int( math.ceil(maxStack/size) ) 
		offset = -1 * offset_size * int(size)
		#print offset_size, offset, offset%int(size)
		assert ( int(offset) % int(size) == 0 )
		
		# check that the offset value can hold in 4 bytes, since the generated code uses ECX to hold it
		assert ( offset_size <= 4294967295 and -4294967295 <= offset_size ) # 4294967295 = 2^32 - 1
		
		# https://pymotw.com/2/struct/
		# formatting https://docs.python.org/3.0/library/struct.html
		# NOTE: I think this will throw an exception if the values don't fall in the expected range given by format (l=long)
		OffsetSize = struct.pack(formatOffsetSize, offset_size) 
		Offset = struct.pack(formatOffset, offset)
		
		# assume these offset are 4 byte long, which is the size of e-reg
		assert ( len(OffsetSize) == 4 and len(Offset) == 4 and "Computed offsets are not 4-byte long" )
		
		return OffsetSize, Offset
	
	# This is my original implementation, ie without using struct.pack
	#def raw_offsets_value_from_stack( self, maxStack, platform ):
	
		## WARNING: we must also change the patching for x86_32, coz I currently assume we use r-registers
		#if platform == "x86_32":
			#raise Exception("Platform not supported yet")
		#elif platform == "x86_64":
			#size = 8.
		
		## WARNING: we use the ceil to make sure everything is erased
		#offset_size = int( math.ceil(maxStack/size) ) 
		#offset = -1 * offset_size * int(size)
		#offset &= (2**32-1)
		##print offset_size, offset, offset%int(size)
		#assert ( int(offset) % int(size) == 0 )
		
		## check that the offset value can hold in 4 bytes, since the generated code uses ECX to hold it
		#assert ( offset_size <= 4294967295 ) # 4294967295 = 2^32 - 1
		
		## get the hex representation, with the padding 0s
		#offset_size_HexBE = hex( int(offset_size) )[2:]
		#offset_HexBE = hex( int(offset) )[2:]
		#offset_size_HexBE = '0'*(int(size)-len(offset_size_HexBE)) + offset_size_HexBE
		#offset_HexBE = '0'*(int(size)-len(offset_HexBE)) + offset_HexBE
		
		##print offset_size_HexBE, offset_HexBE
		#offset_size_HexLE = ''
		#offset_HexLE = ''
		
		## now get the LE version of it
		#assert ( len(offset_size_HexBE) % 2 == 0 )
		#assert ( len(offset_HexBE) % 2 == 0 )
		#if platform == "x86_32" or platform == "x86_64":
			## these are little endian, so need to swap
			#for i in range(len(offset_size_HexBE)/2):
				#offset_size_HexLE += offset_size_HexBE[len(offset_size_HexBE)-(2*i)-2]
				#offset_size_HexLE += offset_size_HexBE[len(offset_size_HexBE)-(2*i)-1]
			
			#for i in range(len(offset_HexBE)/2):
				#offset_HexLE += offset_HexBE[len(offset_HexBE)-(2*i)-2]
				#offset_HexLE += offset_HexBE[len(offset_HexBE)-(2*i)-1]
		#else:
			#raise Exception("Unsupoprted platform") 
		
		#return offset_size_HexLE.decode("hex"), offset_HexLE.decode("hex")
	
	def saveExecutable ( self, fname ):
		assert ( self.patched )
		self.executable.save( fname )
		st = os.stat(fname)
		# stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH for exe for all users. Below is only for owner
		os.chmod(fname, st.st_mode | stat.S_IEXEC)			


def function_info_from_line( line, bulkZeroing ):
	if bulkZeroing:
		match = re.match(r'^(.*)\(([0-9]*)[|](.*)\):(.*)$', line)
		if match == None:
			raise Exception("Invalid file line. Are you sure this a build with bulk register zeroing?")
		fname = match.group(1)
		stackSize = int( match.group(2) )
		RegList = match.group(3).split(',')
		calleeFnames = match.group(4).split(',')
		calleeFnames = [v for v in calleeFnames if len(v)>0 ]	# remove the empty function names
		assert ( stackSize >= 0 )	# that's a given because of the way we matched
		return fname, stackSize, RegList, calleeFnames
	else:
		match = re.match(r'^(.*)\(([0-9]*)\):(.*)$', line)
		if match == None:
			raise Exception("Invalid file line. Are you sure this a build with per-function register zeroing?")
		fname = match.group(1)
		stackSize = int( match.group(2) )
		calleeFnames = match.group(3).split(',')
		calleeFnames = [v for v in calleeFnames if len(v)>0 ]	# remove the empty function names
		assert ( stackSize >= 0 )	# that's a given because of the way we matched
		return fname, stackSize, [], calleeFnames
	

def read_functions_from_file( inmetafiles, bulkZeroing ):
	
	# load the metafiles, including libc and other libs the executable depends on
	# for now, assume there is no function name collision... that obviously won't be true in practice
	# but I'll take care of this after the basic functionalities work
	functionMap = dict()
	sensitiveFunctions = dict()
	
	# ========================================================
	# first add the function configured at the top of this file
	# ========================================================
	
	# TEST REGISTER
	#functionMap["LOLO"] = dict()
	#functionMap["LOLO"]["strlen"] = (11, [], [])
	#functionMap["LOLO"]["scanf"] = (11, [], [])
	
	# add the 'syscall' function
	assert ( BUILTIN_KEY not in functionMap )
	functionMap[BUILTIN_KEY] = dict()
	functionMap[BUILTIN_KEY][ BUILTIN_SYSCALL ] = (0, [], [])
	
	# add functions provided by the compiler's runtime
	# WARNING: what happens when redefined in project??
	assert ( COMPILER_RT_KEY not in functionMap )
	functionMap[COMPILER_RT_KEY] = dict()
	for cfname,cfinfo in COMPILER_RT_FUNCTIONS.iteritems():
		assert ( cfname not in functionMap[COMPILER_RT_KEY] )
		if bulkZeroing:
			functionMap[COMPILER_RT_KEY][cfname] = (cfinfo[0], cfinfo[1], cfinfo[2])
		else:
			functionMap[COMPILER_RT_KEY][cfname] = (cfinfo[0], [], cfinfo[2])	# empty register list
	
	# add functions that musl-libc has in assembly, which we don't inspect in the LLVM passes
	assert ( LIBC_ASM_KEY not in functionMap )
	functionMap[LIBC_ASM_KEY] = dict()
	for cfname,cfinfo in LIBC_ASM_FUNCTION.iteritems():
		assert ( cfname not in functionMap[LIBC_ASM_KEY] )
		if bulkZeroing:
			functionMap[LIBC_ASM_KEY][cfname] = (cfinfo[0], cfinfo[1], cfinfo[2])
		else:
			functionMap[LIBC_ASM_KEY][cfname] = (cfinfo[0], [], cfinfo[2])	# empty register list
		
	# add LLVM intrinsics functions, they appear as llvm.name.*
	#assert ( LLVM_INTRINSICS_KEY not in functionMap )
	#functionMap[LLVM_INTRINSICS_KEY] = dict()
	#for cfname,cfstack,cfcallees,cfregs in LLVM_INTRINSICS_FUNCTIONS.iteritems():
		#assert ( cfname not in functionMap[LLVM_INTRINSICS_KEY] )
		#functionMap[LLVM_INTRINSICS_KEY][cfname] = (cfstack,cfcallees,cfregs)
	
	# add functions from vdso, provided by kernel
	assert ( VDSO_KEY not in functionMap )
	functionMap[VDSO_KEY] = dict()
	for cfname,cfinfo in VDSO_FUNCTION.iteritems():
		assert ( cfname not in functionMap[VDSO_KEY] )
		if bulkZeroing:
			functionMap[VDSO_KEY][cfname] = (cfinfo[0], cfinfo[1], cfinfo[2])
		else:
			functionMap[VDSO_KEY][cfname] = (cfinfo[0], [], cfinfo[2])	# empty register list
		
	
	# ========================================================
	# now add the functions from the metafiles
	# ========================================================
	# We're creating a hash map as functionMap[metafilename][functionName] = [callee1, callee2, ..., calleeN]
	
	# create a dummy entry that will hold all the annotation names, as if these were actual functions
	# this is just to let certain sanitity check pass since I check if a function is indeed defined
	functionMap[ANNOTATED_KEY] = dict()
	
	# this is a hack: the annotation is the vdso function itself
	# WARNING: there might already be some functions annotated with it
	# eg clock_gettime.c of musl sets up a default initalizer before it uses the vdso one: init -> vdso_func. After, vdso_func directly
	# so ANNOTATED_KEY:tag_musl_vdsofname = (0, 0, [vdsoname])
	# eg with "__vdso_time": ANNOTATED_KEY:tag_musl___vdso_time = (0, 0, [__vdso_time,init])
	for vdsoname in functionMap[VDSO_KEY]:
		annoname = vdsoname_to_annotatedname(vdsoname)
		# Note: at this point there is no function yet in functionMap[VDSO_KEY]
		assert( annoname not in functionMap[ ANNOTATED_KEY ] )
		functionMap[ ANNOTATED_KEY ][annoname] = ( 0, [], [vdsoname] )	# this is a dummy entry that should be like a "nop"
			
	for fn in inmetafiles:
		
		# create the hash map for the metafile
		functionMap[ fn ] = dict()
		sensitiveFunctions[ fn ] = []
		
		#print fn
		with open(fn) as f:
			content = f.readlines()
		# the file contains lines with format:
		# fname( stackSize ):fname2,fname3,fname4
		# where stackSize is a positive number, and the list of fcuntions are called by fname
		for line in content:
			
			#print line
			fname, stackSize, regList, calleeFnames = function_info_from_line( line, bulkZeroing )
			
			
			# WARNING: we assume sensitive info comes BEFORE that type annotation
			# see backend pass, that's what we do
			
			# check if the function is marked sensitive
			isSensitive = False
			if fname[:len(SENSITIVE_FUNCTION)] == SENSITIVE_FUNCTION:
				fname = fname[len(SENSITIVE_FUNCTION)+3:]	# there's an extra _?_ so we add 3
				assert ( len(fname) )
				isSensitive = True
				
			
			# check if there is some type annotation. Only one is supported
			# @type_annotate_XXX_?_functionName
			fAnnotation = ""
			if fname[:len(ANNOTATED_FUNCTION)] == ANNOTATED_FUNCTION:
				fname = fname[len(ANNOTATED_FUNCTION)+1:]	# there's an extra _ so we add 1
				assert ( len(fname) )
				pos = fname.find( "_?_" )
				assert ( pos != -1 and "Cannot find end of type annotation" )
				fAnnotation = fname[:pos]
				fname = fname[pos+3:]	# add 3 for _?_
				
				
			#print fname, fAnnotation, calleeFnames
			
			
			# now that we have the exact function name, add it to the relevant arrays
			if isSensitive:
				sensitiveFunctions[ fn ] += [ fname ]
				
			#if len(fAnnotation):
				## annotated functions are special: they belong to no particular metafile, and their callees
				## are the function that are annotated with a particular name
				#AnnotationKey = formatForAnnotatedKey( fAnnotation )
				#if AnnotationKey not in functionMap:
					#functionMap[AnnotationKey] = dict()
				
			
			#print 'function:', fname
			#print 'stackSize:', stackSize
			# check there's no empty function in callee list
			assert ( "" not in calleeFnames )
			
			# this will abort if a project redefines a function from libraries it depends on
			# let's see if this happens in practice. If so, we'll need to resolve symbols somewhat :)
			if fname in functionMap[ fn ]:
				msg = "Function '" + fname + "' is redefined in file" + fn
				raise Exception(msg)
				
			# make sure we don't have a type-annotation and function-name clash
			# below takes care only of the case when the function does not have the annotation as its name
			# the case where function name is the same as its annotation will be caught as a loop
			# =================================================================
			# 1. check that the function name is not in the type annotation
			if fname in functionMap[ ANNOTATED_KEY ]:
				msg = "'" + fname + "': both a function name and a type annotation"
				raise Exception(msg)
			# 2. check that the annotation is not already a function
			if len(fAnnotation):
				plainFunctionList = set( [lv for k,v in functionMap.iteritems() for lv in v if k!=ANNOTATED_KEY] )	# remove annotations frm the lot
				if fAnnotation in plainFunctionList:
					msg = "'" + fAnnotation + "': both a type annotation and a function name"
					raise Exception(msg)
			
			functionMap[ fn ][fname] = ( stackSize, regList, calleeFnames )
			
			# also add the function for the group of annotated functions
			# this is in fact a complete duplicate of data we already have added in previous line...
			# and we copy it twice...
			# TODO: remove the waste of memory
			if len( fAnnotation ):
				if fAnnotation not in functionMap[ ANNOTATED_KEY ]:
					functionMap[ ANNOTATED_KEY ][fAnnotation] = ( 0, [], [fname] )	# this is a dummy entry that should be like a "nop"
				else:
					currentFList = functionMap[ ANNOTATED_KEY ][fAnnotation][2]
					functionMap[ ANNOTATED_KEY ][fAnnotation] = ( 0, [], currentFList + [fname] )
	
	print annoname, functionMap[ ANNOTATED_KEY ][annoname]
	
	return functionMap, sensitiveFunctions

def validateUniqueMetafiles( inmetafiles ) :
	dups = set( [x for x in inmetafiles if inmetafiles.count(x) > 1] )
	if len( dups ):
		sep = "\n\t- "
		msg = "Duplicate metafiles found:" + sep + sep.join(dups)
		raise Exception(msg)


def validateMetafilesExist( inmetafiles ):
	for fname in inmetafiles:
		if not os.path.exists(fname):
			msg = "Metafile '" + fname + "' does not exist"
			raise Exception(msg)

def printFunctions(msg, funcList):
	print msg
	
	for metafile, funcs in funcList.iteritems(): 
		if len( funcs ) == 0:
			continue
		print "From file", os.path.basename(metafile), ":"
		for fname in funcs:
			print "\t-", fname



def main(options):
	
	inobject = os.path.realpath(options.inobject)
	outobject = os.path.realpath(options.outobject)
	platform = options.platform
	ssu = options.ssu
	brz = options.brz
	libc = options.libc
	inmetafiles = [ os.path.realpath( inmetafn ) for inmetafn in options.inmetafiles if len(inmetafn)>0 ]
	
	# validate right now that metafiles exist, so we don't waste time later
	validateMetafilesExist( inmetafiles )
			
	# validate the metafile names, we we're using them s keys into a map. They MUST ALL be different
	validateUniqueMetafiles( inmetafiles )
	
	# load the dispatcher
	dispatcher = Dispatcher( inobject )
	
	# load the functions
	functionMap, sensitiveFunctions = read_functions_from_file( inmetafiles, brz )
	
	#printFunctions("hello\n", sensitiveFunctions)
	
	# patch each sensitive function
	for metafile, funcs in sensitiveFunctions.iteritems(): 
		for sensitiveFunc in funcs:
			
			print "Processing sensitive function", sensitiveFunc
			maxPath, maxStack, maxRegs = walkTree(functionMap, metafile, libc, sensitiveFunc, [ os.path.basename(metafile) + ":" + sensitiveFunc ])
			
			if platform == "x86_64":
				maxStack += RED_ZONE
				print "Red Zone fix applied:", maxStack-RED_ZONE, "->", maxStack
			
			maxStack += ssu
			print "Signal handler fix applied:", maxStack-ssu, "->", maxStack
			
			print bcolors.HEADER + "Patch for function:", bcolors.UNDERLINE + sensitiveFunc + bcolors.ENDC, bcolors.ENDC
			print 'maxStack:', bcolors.GREEN, maxStack, hex(maxStack), bcolors.ENDC
			print 'maxPath:' + maxPath
			print 'maxRegs:' + bcolors.GREEN + ",".join(maxRegs) + bcolors.ENDC
			
			# patch the binary
			dispatcher.patchFunctionWith( sensitiveFunc, maxStack, maxRegs, platform )
		
	
	# save to new file, once only
	dispatcher.saveExecutable( outobject )

class bcolors:
	# http://kishorelive.com/2011/12/05/printing-colors-in-the-terminal/
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    GREEN = '\033[92m'
    TURQUOISE = '\033[96m'
    YELLOW = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
   
    		
if __name__ == '__main__':
		
	parser = OptionParser()
	(options, args) = get_options(parser)
	check_options(parser, options, args)
	
	try:
		
		main(options)
		
	except:
		print "exception caught"
		traceback.print_exc(file=sys.stdout)
	#finally:
		#print 'finally'

	print 'Done ...'
