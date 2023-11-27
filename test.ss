(library (test)
  (export test)
  (import (scheme) (base))

  (define-syntax test
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $spec ...)
          #`(begin
            #,@(map
              (lambda ($spec)
                #`(let ()
                  (load
                    #,(string-append
                      (apply string-append
                        (intercalate
                          (map symbol->string (syntax->datum $spec))
                          "/"))
                      "-test.ss"))))
                (syntax->list #'($spec ...))))))))
)
