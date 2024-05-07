(library (llvm api)
  (export
    llvm-with-module
    llvm-dump-module
    llvm-dump-type
    llvm-int32-type
    llvm-function-type
    llvm-add-function
    llvm-append-basic-block

    llvm-with-builder
    llvm-position-builder-at-end
    llvm-build-add
    llvm-build-ret

    llvm-get-param

    llvm-verify-module
    llvm-with-execution-engine-for-module

    llvm-link-in-mcjit
    llvm-link-in-interpreter)
  (import
    (scheme)
    (data)
    (syntax)
    (syntaxes)
    (system)
    (lets)
    (procedure)
    (llvm foreign)
    (dynamic-wind)
    (foreign)
    (throw))

  (define-rule-syntax (with-vector-ftype-pointer-and-count (id length ftype vector) body)
    (lets
      (vector-var vector)
      (length (vector-length vector-var))
      (with-foreign-alloc-0 (ptr (* (ftype-sizeof ftype) length))
        (let ((id (make-ftype-pointer ftype ptr)))
          (repeat-indexed length index
            (ftype-set! ftype () id index (vector-ref vector-var index)))
          body))))

  (define-rule-syntax (llvm-with-module (id name) body)
    (with-dynamic-wind
      (id (LLVMModuleCreateWithName name))
      body
      (LLVMDisposeModule id)))

  (define-rule-syntax (llvm-dump-module mod)
    (LLVMDumpModule mod))

  (define-rule-syntax (llvm-add-function mod name type)
    (LLVMAddFunction mod name type))

  (define-rule-syntax (llvm-append-basic-block fn name)
    (LLVMAppendBasicBlock fn name))

  (define-rule-syntax (llvm-int32-type)
    (LLVMInt32Type))

  (define-rules-syntax
    ((llvm-function-type result params-vec vararg)
      (with-vector-ftype-pointer-and-count (params count LLVMTypeRef params-vec)
        (LLVMFunctionType result params count vararg)))
    ((llvm-function-type result params-vec)
      (llvm-function-type result params-vec #f)))

  (define-rule-syntax (llvm-dump-type type)
    (LLVMDumpType type))

  (define-rule-syntax (llvm-with-builder id body)
    (with-dynamic-wind
      (id (LLVMCreateBuilder))
      body
      (LLVMDisposeBuilder id)))

  (define-rule-syntax (llvm-position-builder-at-end builder block)
    (LLVMPositionBuilderAtEnd builder block))

  (define-rule-syntax (llvm-build-add builder lhs rhs name)
    (LLVMBuildAdd builder lhs rhs name))

  (define-rule-syntax (llvm-build-ret builder value)
    (LLVMBuildRet builder value))

  (define-rule-syntax (llvm-get-param fn index)
    (LLVMGetParam fn index))

  (define-rule-syntax (llvm-verify-module mod)
    (LLVMVerifyModule mod 0 0))

  (define-rule-syntax (llvm-link-in-mcjit)
    (LLVMLinkInMCJIT))

  (define-rule-syntax (llvm-link-in-interpreter)
    (LLVMLinkInInterpreter))

  (define-rule-syntax (llvm-with-execution-engine-for-module (engine mod) body)
    (with-foreign-alloc
      (engine-ptr (ftype-sizeof uptr))
      (error-ptr (ftype-sizeof uptr))
      (with-dynamic-wind
        (engine
          (lets
            (ok?
              (LLVMCreateExecutionEngineForModule
                (make-ftype-pointer LLVMExecutionEngineRef engine-ptr)
                mod
                error-ptr))
            (if ok?
              (ftype-ref uptr () engine-ptr 0)
              (throw create-execution-engine-error))))
        body
        (LLVMDisposeExecutionEngine engine))))
)
