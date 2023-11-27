(library (define-syntax)
  (export
    define-syntax-rule
    define-syntax-case
    define-aux-keyword)
  (import (scheme))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((_ (name param ...) body)
        (define-syntax name
          (syntax-rules ()
            ((_ param ...) body))))))

  (define-syntax define-syntax-case
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($name $param ...) $body)
          #`(define-syntax-case ($name $param ...) () $body))
        ((_ ($name $param ...) $keywords $body)
          #`(define-syntax-case $name $keywords
            ((_ $param ...) $body)))
        ((_ $name $keywords $case ...)
          (let (($tmp (car (generate-temporaries `(tmp)))))
            #`(define-syntax $name
              (lambda (#,$tmp)
                (syntax-case #,$tmp $keywords
                  $case ...))))))))

  (define-syntax-rule (define-aux-keyword aux)
    (define-syntax aux
      (lambda (stx)
        (syntax-error stx "misplaced aux keyword"))))
)
