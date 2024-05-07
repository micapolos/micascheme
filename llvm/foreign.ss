(library (llvm foreign)
  (export
    LLVMModuleRef
    LLVMTypeRef
    LLVMValueRef
    LLVMBasicBlockRef

    LLVMModuleCreateWithName
    LLVMDisposeModule
    LLVMDumpModule
    LLVMAddFunction
    LLVMAppendBasicBlock

    LLVMInt32Type
    LLVMFunctionType
    LLVMDumpType)
  (import (scheme) (syntax))

  (define llvm
    (load-shared-object "/usr/local/opt/llvm/lib/libLLVM-C.dylib"))

  (define-ftype LLVMModuleRef uptr)
  (define-ftype LLVMTypeRef uptr)
  (define-ftype LLVMValueRef uptr)
  (define-ftype LLVMBasicBlockRef uptr)

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
)
