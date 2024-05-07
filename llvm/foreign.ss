(library (llvm foreign)
  (export
    LLVMModuleRef
    LLVMTypeRef
    LLVMValueRef
    LLVMBasicBlockRef
    LLVMBuilderRef
    LLVMExecutionEngineRef

    LLVMModuleCreateWithName
    LLVMDisposeModule
    LLVMDumpModule
    LLVMAddFunction
    LLVMAppendBasicBlock

    LLVMInt32Type
    LLVMFunctionType
    LLVMDumpType

    LLVMCreateBuilder
    LLVMPositionBuilderAtEnd
    LLVMDisposeBuilder

    LLVMBuildAdd
    LLVMBuildRet

    LLVMGetParam

    LLVMVerifyModule
    LLVMCreateExecutionEngineForModule
    LLVMDisposeExecutionEngine)
  (import (scheme) (syntax))

  (define llvm
    (load-shared-object "/usr/local/opt/llvm/lib/libLLVM-C.dylib"))

  (define-ftype LLVMModuleRef uptr)
  (define-ftype LLVMTypeRef uptr)
  (define-ftype LLVMValueRef uptr)
  (define-ftype LLVMBasicBlockRef uptr)
  (define-ftype LLVMBuilderRef uptr)
  (define-ftype LLVMExecutionEngineRef uptr)

  (define-case-syntax (define-llvm (name param ...) result)
    #`(define name
      (foreign-procedure
        #,(symbol->string (datum name))
        (param ...)
        result)))

  (define-llvm (LLVMModuleCreateWithName string) LLVMModuleRef)
  (define-llvm (LLVMDisposeModule LLVMModuleRef) void)
  (define-llvm (LLVMDumpModule LLVMTypeRef) void)
  (define-llvm (LLVMAddFunction LLVMModuleRef string LLVMTypeRef) LLVMValueRef)
  (define-llvm (LLVMAppendBasicBlock LLVMValueRef string) LLVMBasicBlockRef)

  (define-llvm (LLVMInt32Type) LLVMTypeRef)
  (define-llvm (LLVMFunctionType LLVMTypeRef (* LLVMTypeRef) unsigned boolean) LLVMTypeRef)
  (define-llvm (LLVMDumpType LLVMTypeRef) void)

  (define-llvm (LLVMCreateBuilder) LLVMBuilderRef)
  (define-llvm (LLVMPositionBuilderAtEnd LLVMBuilderRef LLVMBasicBlockRef) void)
  (define-llvm (LLVMDisposeBuilder LLVMBuilderRef) void)
  (define-llvm (LLVMBuildAdd LLVMBuilderRef LLVMValueRef LLVMValueRef string) LLVMValueRef)
  (define-llvm (LLVMBuildRet LLVMBuilderRef LLVMValueRef) LLVMValueRef)

  (define-llvm (LLVMGetParam LLVMValueRef unsigned) LLVMValueRef)

  (define-llvm (LLVMVerifyModule LLVMModuleRef unsigned uptr) boolean)

  (define-llvm (LLVMCreateExecutionEngineForModule (* LLVMExecutionEngineRef) LLVMModuleRef uptr) boolean)
  (define-llvm (LLVMDisposeExecutionEngine LLVMExecutionEngineRef) void)
)
