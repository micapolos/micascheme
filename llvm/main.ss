(import (micascheme) (llvm api))

(llvm-with-module (foo-module "foo")
  (lets
    (sum-function
      (llvm-add-function foo-module "sum"
        (llvm-function-type
          (llvm-int32-type)
          (vector (llvm-int32-type) (llvm-int32-type)))))
    (entry-block
      (llvm-append-basic-block sum-function "entry"))
    (run
      (llvm-with-builder builder
        (llvm-position-builder-at-end builder entry-block)
        (lets
          (tmp-value
            (llvm-build-add builder
            (llvm-get-param sum-function 0)
            (llvm-get-param sum-function 1)
            "tmp"))
          (run
            (llvm-build-ret builder tmp-value))))
        (llvm-dump-module foo-module)
        (llvm-verify-module foo-module)
        (llvm-link-in-mcjit)
        (llvm-with-execution-engine-for-module (engine foo-module)
          (displayln
            (format "10 + 20 = ~a"
              (llvm-generic-value-to-int
                (llvm-run-function engine sum-function
                  (vector
                    (llvm-create-generic-value-of-int (llvm-int32-type) 10 #f)
                    (llvm-create-generic-value-of-int (llvm-int32-type) 20 #f)))
                #f)))))))
