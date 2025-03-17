(library (datums)
  (export make get is? when)
  (import (micascheme))

  (define (datum-name $datum)
    (or
      (switch $datum
        ((number? $number) `number)
        ((string? $string) `string)
        ((boolean? $boolean) `boolean)
        ((procedure? $procedure) `procedure)
        ((pair? $pair)
          (switch (car $pair)
            ((symbol? $symbol) $symbol)
            ((else _) #f))))
      (throw datum-name $datum)))

  (define (datum-is? $name $datum)
    (symbol=? (datum-name $datum) $name))

  (define (datum-select-else $name $datum $else)
    (if (symbol=? (datum-name $datum) $name) $datum ($else)))

  (define (datums-select $name $datums)
    (switch $datums
      ((pair? $pair) 
        (datum-select-else $name (car $pair)
          (lambda () (datums-select $name (cdr $datums)))))
      ((else _) (throw no-field $name))))

  (define (datum-get $name $datum)
    (switch $datum
      ((pair? $pair) (datums-select $name (cdr $pair)))
      ((else _) (throw not-record $datum))))

  (define-syntax (make $syntax)
    (syntax-case $syntax ()
      ((_ $name $field ...) (identifier? #`$name)
        #`(list (quote $name) $field ...))))

  (define-syntax (get $syntax)
    (syntax-case $syntax ()
      ((_ $name $target) (identifier? #`$name)
        #`(datum-get (quote $name) $target))))

  (define-syntax (is? $syntax)
    (syntax-case $syntax ()
      ((_ $name $target) (identifier? #`$name)
        #`(datum-is? (quote $name) $target))))

  (define-syntax (when $syntax)
    (syntax-case $syntax (is? else)
      ((_ expr ((is? name var) body ...) ... ((else else-var) else-body ...))
        (let ((tmp (car (generate-temporaries `(tmp)))))
          #`(let ((#,tmp expr))
            (cond
              ((is? (quote name) #,tmp)
                (let ((var #,tmp)) body ...)) ...
              (else
                (let ((else-var #,tmp)) else-body ...))))))
      ((_ expr ((is? name var) body ...) ...)
        (let ((tmp (car (generate-temporaries `(tmp)))))
          #`(let ((#,tmp expr))
            (cond
              ((is? (quote name)  #,tmp)
                (let ((var #,tmp)) body ...)) ...))))))
)
