(import (micascheme) (llvm api))

(llvm-with-module
  (foo-module "foo")
  (lets
    (sum-function
      (llvm-add-function foo-module "sum"
        (llvm-function-type
          (llvm-int32-type)
          (vector (llvm-int32-type) (llvm-int32-type)))))
    (entry-block
      (llvm-append-basic-block sum-function "entry"))
    (llvm-dump-module foo-module))
)
