(library (asm expr-lang)
  (export number string boolean lambda typed plus plus/typed minus/typed expr-of type=?)
  (import (except (micascheme) string lambda))

  (define-keywords number string boolean lambda)

  (define-rule-syntax (plus a b)
    (plus/typed
      (typed (syntax-eval #'a))
      (typed (syntax-eval #'b))))

  (define (plus/typed $a $b)
    (syntax-case $a (number string)
      ((number expr)
        #`(number (+ expr #,(expr-of #'number $b))))
      ((string expr)
        #`(string (string-append expr #,(expr-of #'string $b))))
      ((type expr)
        (syntax-error #'expr
          (format "invalid type ~s, expected ~s or ~s in"
            (syntax->datum #'type)
            (syntax->datum #'number)
            (syntax->datum #'string))))))

  (define (minus/typed $a $b)
    #`(number
      (-
        #,(expr-of #'number $a)
        #,(expr-of #'number $b))))

  (define (typed $syntax)
    (syntax-case $syntax ()
      (x
        (boolean? (datum x))
        #'(boolean x))
      (x
        (number? (datum x))
        #'(number x))
      (x
        (string? (datum x))
        #'(string x))
      (x #'x)))

  (define (expr-of $type $typed)
    (syntax-case $typed ()
      ((type expr)
        (cond
          ((type=? #'type $type) #'expr)
          (else
            (syntax-error #'expr
              (format "invalid type ~s, expected ~s in"
                (syntax->datum #'type)
                (syntax->datum $type))))))))

  (define (type=? $type-a $type-b)
    (syntax-case? $type-a (number string boolean)
      (number
        (syntax-case? $type-b (number)
          (number #t)))
      (string
        (syntax-case? $type-b (string)
          (string #t)))
      (boolean
        (syntax-case? $type-b (boolean)
          (boolean #t)))))
)
