(library (llvm api)
  (export
    llvm-with-module
    llvm-dump-module
    llvm-dump-type
    llvm-int32-type
    llvm-function-type
    llvm-add-function
    llvm-append-basic-block)
  (import
    (scheme)
    (data)
    (syntax)
    (syntaxes)
    (system)
    (lets)
    (procedure)
    (llvm foreign)
    (binder))

  (define-rule-syntax (with-rtti (id create dispose) body ...)
    (let ()
      (define id)
      (dynamic-wind
        (lambda () (set! id create))
        (lambda () body ...)
        (lambda () dispose))))

  (define-rule-syntax (with-vector-ftype-pointer-and-count (id length ftype vector) body ...)
    (lets
      (vector-var vector)
      (length (vector-length vector-var))
      (alloc? (not (zero? length)))
      (with-rtti
        (ptr
          (if alloc? (foreign-alloc (* (ftype-sizeof ftype) length)) 0)
          (if alloc? (foreign-free ptr)))
        (let ((id (make-ftype-pointer ftype ptr)))
          (repeat-indexed length index
            (ftype-set! ftype () id index (vector-ref vector-var index)))
          body ...))))

  (define-rule-syntax (llvm-with-module (id name) body ...)
    (with-rtti
      (id
        (LLVMModuleCreateWithName name)
        (LLVMDisposeModule id))
      body ...))

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
)
