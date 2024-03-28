(library (labs js)
  (export parse-js)
  (import (micascheme))

  (define (parse-js $syntax)
    (syntax-case $syntax (set! if begin lambda let letrec $primitive + - *)
      ($id
        (identifier? #'$id)
        (symbol->string (syntax->datum #'$id)))
      ((set! $x $e)
        (string-append
          (parse-js #'$x)
          " = "
          (parse-js #'$e)))
      ((if $e1 $e2)
        (string-append
          "if ("
          (parse-js #'$e1)
          ") "
          (parse-js #'$e2)))
      ((if $e1 $e2 $e3)
        (string-append
          "if ("
          (parse-js #'$e1)
          ") "
          (parse-js #'$e2)
          " else "
          (parse-js #'$e3)))
      ((begin $e1 ... $e2)
        (string-append
          "{ "
          (apply string-append
            (map
              (lambda ($js) (string-append $js "; "))
              (map parse-js (syntax->list #'($e1 ...)))))
          (parse-js #'$e2)
          " }"))
      ((lambda ($x ...) $body1 ... $body2)
        (string-append
          "(("
          (apply string-append
            (intercalate
              (map parse-js (syntax->list #'($x ...)))
              ", "))
          ") => "
          (parse-js #`(begin $body1 ... $body2))
          ")"))
      ((let (($x $e) ...) $body1 ... $body2)
        (string-append
          "{ "
          (apply string-append
            (map
              (lambda ($x $e)
                (string-append
                  "let "
                  (parse-js $x)
                  " = "
                  (parse-js $e)
                  "; "))
              (syntax->list #'($x ...))
              (syntax->list #'($e ...))))
          (apply string-append
            (map
              (lambda ($js) (string-append $js "; "))
              (map parse-js (syntax->list #'($body1 ...)))))
          (parse-js #'$body2)
          " }"))
      ((letrec* (($x $e) ...) $body1 ... $body2)
        (parse-js #`(let (($x $e) ...) $body1 ... $body2)))
      ((($primitive _ +) $e ...)
        (apply string-append
          (intercalate
            (map parse-js (syntax->list #'($e ...)))
            " + ")))
      ((($primitive _ -) $e ...)
        (apply string-append
          (intercalate
            (map parse-js (syntax->list #'($e ...)))
            " - ")))
      ((($primitive _ *) $e ...)
        (apply string-append
          (intercalate
            (map parse-js (syntax->list #'($e ...)))
            " * ")))
      ((($primitive _ <) $e ...)
        (apply string-append
          (intercalate
            (map parse-js (syntax->list #'($e ...)))
            " < ")))
      (($e0 $e1 ...)
        (string-append
          (parse-js #'$e0)
          (apply string-append
            (intercalate
              (map parse-js (syntax->list #'($e1 ...)))
              ", "))))
      ($other
        (format "~s" (datum $other)))))
)
