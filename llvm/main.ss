(import (micascheme) (llvm api))

(llvm-with-module (mod "foo")
  (lets
    (fn
      (llvm-add-function mod "sum"
        (llvm-function-type
          (llvm-int32-type)
          (vector (llvm-int32-type) (llvm-int32-type)))))
    (block (llvm-append-basic-block fn "entry"))
    (llvm-dump-module mod))
)
