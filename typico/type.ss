(library (typico type)
  (export
    type?
    primitive-type primitive-type?
    function-type function-type? function-type-param-types function-type-result-type
    forall-type forall-type? forall-type-arity forall-type-type
    variable-type variable-type? variable-type-index
    application-type application-type? application-type-type application-type-args
    expander-type expander-type? expander-type-proc
    type->datum
    type=?)
  (import (micascheme))

  (data (primitive-type gensym datum))
  (data (function-type param-types result-type))
  (data (forall-type arity type))
  (data (variable-type index))
  (data (application-type type args))
  (data (expander-type proc))

  (define type?
    (or-predicate
      primitive-type?
      function-type?
      forall-type?
      variable-type?
      application-type?
      expander-type?))

  (define (type->datum $type)
    (switch-exhaustive $type
      ((primitive-type? $primitive-type)
        (primitive-type-datum $primitive-type))
      ((function-type? $function-type)
        `(function
          (
          ,@(map*
            type->datum
            (lambda ($type) `(,(type->datum $type) ...))
            (function-type-param-types $function-type)))
          ,(type->datum (function-type-result-type $function-type))))
      ((forall-type? $forall-type)
        `(forall
          ,(forall-type-arity $forall-type)
          ,(type->datum (forall-type-type $forall-type))))
      ((variable-type? $variable-type)
        `(variable ,(variable-type-index $variable-type)))
      ((application-type? $application-type)
        `(
          ,(type->datum (application-type-type $application-type))
          ,@(map type->datum (application-type-args $application-type))))
      ((expander-type? $expander-type)
        `(expander ,(expander-type-proc $expander-type)))))

  (define (type=? $type-a $type-b)
    (equal? $type-a $type-b))
)
