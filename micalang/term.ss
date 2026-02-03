(library (micalang term)
  (export
    variable variable? variable-index
    application application? application-lhs application-rhs
    pi pi? pi-param pi-procedure
    branch branch? branch-cond branch-lhs branch-rhs

    depth-term->datum
    check-term->datum

    term-neutral?
    term-apply)
  (import (except (micascheme) pi))

  (data (variable index))
  (data (application lhs rhs))
  (data (pi param procedure))
  (data (branch cond lhs rhs))

  (define (index->symbol $index)
    (string->symbol (format "v~a" $index)))

  (define (depth-term->datum $depth $term)
    (switch $term
      ((variable? $variable)
        (index->symbol (- $depth (variable-index $variable))))
      ((procedure? $procedure)
        (lets
          ($symbol (index->symbol $depth))
          `(lambda (,$symbol)
            ,(depth-term->datum (+ $depth 1) ($procedure $symbol)))))
      ((application? $application)
        `(
          ,(depth-term->datum $depth (application-lhs $application))
          ,(depth-term->datum $depth (application-rhs $application))))
      ((pi? $pi)
        (lets
          ($param-datum (depth-term->datum $depth (pi-param $pi)))
          ($symbol (index->symbol $depth))
          ($procedure (pi-procedure $pi))
          ($body-depth (+ $depth 1))
          ($body-datum (depth-term->datum $body-depth ($procedure $symbol)))
          ($hole-body-datum (depth-term->datum $body-depth ($procedure 'hole)))
          `(pi
            ,(if (equal? $body-datum $hole-body-datum)
              $param-datum
              `(,$symbol : ,$param-datum))
            ,$body-datum)))
      ((branch? $branch)
        `(if
          ,(depth-term->datum $depth (branch-cond $branch))
          ,(depth-term->datum $depth (branch-lhs $branch))
          ,(depth-term->datum $depth (branch-rhs $branch))))
      ((else $other)
        $other)))

  (define-rule-syntax (check-term->datum in out)
    (check (equal? (depth-term->datum 0 in) 'out)))

  (define term-neutral? (or? variable? application? branch?))

  (define (term-apply $lhs $rhs)
    (switch $lhs
      ((procedure? $procedure)
        (if (term-neutral? $rhs)
          (application $procedure $rhs)
          ($procedure $rhs)))
      ((pi? $pi)
        ((pi-procedure $pi) $rhs))
      ((else $other)
        (application $other $rhs))))
)
