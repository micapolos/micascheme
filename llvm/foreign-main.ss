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
  ($engine-addr (foreign-alloc (ftype-sizeof LLVMExecutionEngineRef)))
  ($engine-ptr (make-ftype-pointer LLVMExecutionEngineRef $engine-addr))
  ($error-addr (foreign-alloc (ftype-sizeof uptr)))
  ($error? (LLVMCreateExecutionEngineForModule $engine-ptr $module $error-addr))
  ($engine (if $error? (throw dupa) (ftype-ref LLVMExecutionEngineRef () $engine-ptr 0)))
  (run (foreign-free $error-addr))
  (run (foreign-free $engine-addr))
  ($lhs-arg (LLVMCreateGenericValueOfInt (LLVMInt32Type) 10 #f))
  ($rhs-arg (LLVMCreateGenericValueOfInt (LLVMInt32Type) 20 #f))
  ($args-addr (foreign-alloc (* 2 (ftype-sizeof LLVMGenericValueRef))))
  ($args-ptr (make-ftype-pointer LLVMGenericValueRef $args-addr))
  (run (ftype-set! LLVMGenericValueRef () $args-ptr 0 $lhs-arg))
  (run (ftype-set! LLVMGenericValueRef () $args-ptr 1 $rhs-arg))
  ($result (LLVMRunFunction $engine $sum-function 2 $args-ptr))
  (run (foreign-free $args-addr))
  (run (displayln (format "10 + 20 = ~a" (LLVMGenericValueToInt $result #f))))
  (run (LLVMDisposeModule $module))
  ;(run (LLVMDisposeExecutionEngine $engine))
  (void))
