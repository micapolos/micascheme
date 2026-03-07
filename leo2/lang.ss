(library (leo2 lang)
  (export
    native lambda apply print
    ; term->lang
    ; check-lang=?
    )
  (import
    (rename (except (leo2 base) lets apply) (lambda %lambda))
    (prefix (leo2 term) %)
    (leo2 datum)
    (leo2 symbol)
    (leo2 normalize))

  (define-rules-syntaxes
    ((native n)
      (%native n))
    ((lambda body)
      body)
    ((lambda id . x)
      (identifier? #'id)
      (%lambda (id) (lambda . x)))
    ((lambda (id expr) . x)
      (identifier? #'id)
      (%application (lambda id . x) expr))
    ((apply fn)
      fn)
    ((apply fn arg . x)
      (apply (%application fn arg) . x))
    ((print x ...)
      (begin
        (pretty-print (term->datum (normalize x))) ...)))

  ; (define (term->lang $term)
  ;   (depth-term->lang 0 $term))

  ; (define (depth-term->lang $depth $term)
  ;   (switch-exhaustive $term
  ;     ((native? $native)
  ;       `(native ,(native->datum (native-ref $native))))
  ;     ((variable? $variable)
  ;       (lets
  ;         ($index (variable-index $variable))
  ;         (if (>= $index $depth)
  ;           `(variable ,$index)
  ;           (depth->symbol $index))))
  ;     ((lambda? $lambda)
  ;       (lets
  ;         ($symbol (depth->symbol $depth))
  ;         `(lambda ,(depth->symbol $depth)
  ;           ,(depth-term->datum
  ;             (+ $depth 1)
  ;             ($lambda (variable $depth))))))
  ;     ((lambda-type? $lambda-type)
  ;       `(lambda-type
  ;         ,(depth-term->datum $depth (lambda-type-param $lambda-type))
  ;         ,(depth-term->datum $depth (lambda-type-lambda $lambda-type))))
  ;     ((application? $application)
  ;       `(apply
  ;         ,(depth-term->datum $depth (application-lhs $application))
  ;         ,(depth-term->datum $depth (application-rhs $application))))))

  ; (define-rule-syntax (check-lang=? in out)
  ;   (check
  ;     (equal?
  ;       (term->lang in)
  ;       (term->lang out))))
)
