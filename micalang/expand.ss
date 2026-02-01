(library (micalang expand)
  (export var-count expand-typed)
  (import (micascheme))

  (define var-count (make-thread-parameter #f))

  (define (genvar)
    (lets
      ($var-count (var-count))
      (if $var-count
        (begin
          (var-count (+ $var-count 1))
          (string->symbol (string-append "v" (number->string $var-count))))
        (gensym))))

  (define (expand-typed $environment $datum/annotation)
    (switch (datum/annotation-expression $datum/annotation)
      ((boolean? $boolean)
        (list 'boolean $boolean))
      ((number? $number)
        (list 'number $number))
      ((string? $string)
        (list 'string $string))
      ((symbol? $symbol)
        (list `',$symbol #f))
      ((pair? $pair)
        (case (car $pair)
          ((the)
            (lets
              ($type (cadr $pair))
              ($entry
                (or
                  (assv $type $environment)
                  (syntax-error $type "undefined")))
              ($variable (cadr $entry))
              (list $type $variable)))
          ((lambda)
            (lets
              ($types (cadr $pair))
              ($body (caddr $pair))
              ($ids (ordered-map (lambda (_) (genvar)) $types))
              ($environment (append (map list $types $ids) $environment))
              ($typed-body (expand-typed $environment $body))
              `(
                (lambda (,@$types) ,(car $typed-body))
                (lambda (,@$ids) ,(cadr $typed-body)))))))
      ((else _)
        (syntax-error $datum/annotation "dupa"))))
)

