(import (micascheme) (llvm foreign) (llvm api))

(lets
  ($module (LLVMModuleCreateWithName "my-module"))
  ($sum-function
    (llvm-add-function $module "sum"
      (llvm-function-type
        (llvm-int32-type)
        (vector (llvm-int32-type) (llvm-int32-type)))))
  ($entry-block (llvm-append-basic-block $sum-function "entry"))
  ($builder (LLVMCreateBuilder))
  (run (llvm-position-builder-at-end $builder $entry-block))
  ($tmp-value
    (llvm-build-add $builder
      (llvm-get-param $sum-function 0)
      (llvm-get-param $sum-function 1)
      "tmp"))
  ($ret-value (llvm-build-ret $builder $tmp-value))
  (run (LLVMDisposeBuilder $builder))
  (run (LLVMDumpModule $module))
  (run (LLVMVerifyModule $module 0 0))
  (run (LLVMLinkInMCJIT))
  ($engine-addr (foreign-alloc (ftype-sizeof LLVMExecutionEngineRef)))
  ($engine-ptr (make-ftype-pointer LLVMExecutionEngineRef $engine-addr))
  ($error-addr (foreign-alloc (ftype-sizeof uptr)))
  ($error? (LLVMCreateExecutionEngineForModule $engine-ptr $module $error-addr))
  ($engine (if $error? (throw 'dupa) (ftype-ref LLVMExecutionEngineRef () $engine-ptr 0)))
  (run
    (displayln
      (format "10 + 20 = ~a"
        (llvm-generic-value-to-int
          (llvm-run-function $engine $sum-function
            (vector
              (llvm-create-generic-value-of-int (llvm-int32-type) 10 #f)
              (llvm-create-generic-value-of-int (llvm-int32-type) 20 #f)))
          #f))))
  ;(run (LLVMDisposeExecutionEngine $engine))
  (run (LLVMDisposeModule $module))
  (void))
