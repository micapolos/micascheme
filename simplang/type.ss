(library (simplang type)
  (export
    boolean integer char arrow macro
    type=?)
  (import (micascheme))

  (define-keywords boolean integer char arrow macro)

  (define (type=? $type-a $type-b)
    (syntax-case? $type-a (boolean integer char string arrow macro)
      (boolean
        (syntax-case? $type-b (boolean)
          (boolean #t)))
      (integer
        (syntax-case? $type-b (integer)
          (integer #t)))
      (char
        (syntax-case? $type-b (char)
          (char #t)))
      (string
        (syntax-case? $type-b (string)
          (string #t)))
      ((arrow (param-a ...) result-a)
        (syntax-case? $type-b (arrow)
          ((arrow (param-b ...) result-b)
            (and
              (= (length #'(param-a ...)) (length #'(param-b ...)))
              (for-all type=? #'(param-a ... result-a) #'(param-b ... result-b))))))
      ((macro proc-a)
        (syntax-case? $type-b (macro)
          ((macro proc-b)
            (equal? #'proc-a #'proc-b))))))
)
