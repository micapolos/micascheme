(library (micalang type)
  (export
    variable variable? variable-index
    application application? application-lhs application-rhs
    pi pi? pi-param pi-procedure
    branch branch? branch-cond branch-lhs branch-rhs

    depth-type->datum
    check-type->datum)
  (import (except (micascheme) pi))

  (data (variable index))
  (data (application lhs rhs))
  (data (pi param procedure))
  (data (branch cond lhs rhs))

  (define (index->symbol $index)
    (string->symbol (format "v~a" $index)))

  (define (depth-type->datum $depth $type)
    (switch $type
      ((variable? $variable)
        (index->symbol (- $depth (variable-index $variable))))
      ((procedure? $procedure)
        (lets
          ($symbol (index->symbol $depth))
          `(lambda (,$symbol)
            ,(depth-type->datum (+ $depth 1) ($procedure $symbol)))))
      ((application? $application)
        `(
          ,(depth-type->datum $depth (application-lhs $application))
          ,(depth-type->datum $depth (application-rhs $application))))
      ((pi? $pi)
        (lets
          ($param-datum (depth-type->datum $depth (pi-param $pi)))
          ($symbol (index->symbol $depth))
          ($procedure (pi-procedure $pi))
          ($body-depth (+ $depth 1))
          ($body-datum (depth-type->datum $body-depth ($procedure $symbol)))
          ($hole-body-datum (depth-type->datum $body-depth ($procedure 'hole)))
          `(pi
            ,(if (equal? $body-datum $hole-body-datum)
              $param-datum
              `(let ,$symbol ,$param-datum))
            ,$body-datum)))
      ((branch? $branch)
        `(if
          ,(depth-type->datum $depth (branch-cond $branch))
          ,(depth-type->datum $depth (branch-lhs $branch))
          ,(depth-type->datum $depth (branch-rhs $branch))))
      ((else $other)
        $other)))

  (define-rule-syntax (check-type->datum in out)
    (check (equal? (depth-type->datum 0 in) 'out)))
)
