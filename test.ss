(library (test)
  (export test)
  (import (scheme) (list))

  (define-syntax test
    (syntax-rules ()
      ((_ $spec ...)
        (begin
          (let ()
            (display (format "Testing ~a\n" '$spec))
            (let (($start-time (current-time 'time-process)))
              (load-program
                (string-append
                  (apply string-append (intercalate (map symbol->string '$spec) "/"))
                  "-test.ss"))))
          ...))))
)
