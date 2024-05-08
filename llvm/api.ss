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
    llvm-create-generic-value-of-int
    llvm-generic-value-to-int
    llvm-run-function

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
      (engine-addr (ftype-sizeof uptr))
      (error-addr (ftype-sizeof uptr))
      (lets
        (engine-ptr (make-ftype-pointer LLVMExecutionEngineRef engine-addr))
        (with-dynamic-wind
          (engine
            (lets
              (error?
                (LLVMCreateExecutionEngineForModule
                  engine-ptr
                  mod
                  error-addr))
              (if (not error?)
                (ftype-ref LLVMExecutionEngineRef () engine-ptr 0)
                (let ()
                  ; TODO: Dispose error-ptr message
                  (throw create-execution-engine-error)))))
          body
          (void) ;(LLVMDisposeExecutionEngine engine) - why dispose crashes?
          ))))

  (define-rule-syntax (llvm-create-generic-value-of-int type integer signed?)
    (LLVMCreateGenericValueOfInt type integer signed?))

  (define-rule-syntax (llvm-generic-value-to-int generic-value signed?)
    (LLVMGenericValueToInt generic-value signed?))

  (define-rule-syntax (llvm-run-function engine fn args)
    (with-vector-ftype-pointer-and-count (args-ptr arg-count LLVMGenericValueRef args)
      (LLVMRunFunction engine fn arg-count args-ptr)))
)
