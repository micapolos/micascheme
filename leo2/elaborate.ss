(library (leo2 elaborate)
  (export
    elaborate
    type-elaborate
    check-elaborates)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 equal)
    (leo2 datum))

  (define (elaborate $term)
    (term-switch $term
      ((nothing? _)
       (typed nothing nothing))

      ((anything? _)
       (typed (type 0) anything))

      ((type? $type)
       ;; Canonical: A type elaborates to itself.
       $type)

      ((native? $native)
       (typed anything $native))

      ((native-application? $na)
       (let ([$args (map elaborate (native-application-args $na))])
         (typed anything (native-application (native-application-procedure $na) $args))))

      ((variable? _)
       (typed anything $term))

      ((procedure? $proc)
       (throw type-error "Cannot infer signature for raw procedure" $proc))

      ((signature? $sig)
       (let* ([$param-type (signature-param $sig)]
              [$body-proc  (signature-procedure $sig)]
              [$elaborated-body-proc
               (lambda ($v)
                 (elaborate ($body-proc (typed $param-type $v))))])
         (typed (signature $param-type (lambda ($v) (type 0)))
                (signature $param-type $elaborated-body-proc))))

      ((application? $app)
       (let* ([$lhs (elaborate (application-lhs $app))]
              [$lhs-type (get-type $lhs)])
         (switch $lhs-type
           ((signature? $sig)
            (let ([$rhs (type-elaborate (signature-param $sig) (application-rhs $app))])
              (typed (signature-apply $sig $rhs) (application $lhs $rhs))))
           ((else _)
            (throw type-error "Application LHS must have a signature type" $lhs-type)))))

      ((branch? $branch)
       (let* ([$cond (type-elaborate (native 'Boolean) (branch-condition $branch))]
              [$then (elaborate (branch-consequent $branch))]
              [$else (elaborate (branch-alternate $branch))]
              [$then-type (get-type $then)]
              [$else-type (get-type $else)])
         (if (term=? $then-type $else-type)
             (typed $then-type (branch $cond $then $else))
             (throw type-error "Branch arm type mismatch" (list $then-type $else-type)))))

      ((recursion? $rec)
       (let ([$proc (elaborate (recursion-procedure $rec))])
         (typed (get-type $proc) (recursion (untyped-internal $proc)))))

      ((labeled? $l)
       (let ([$inner (elaborate (labeled-ref $l))])
         (typed (get-type $inner) (labeled (labeled-label $l) $inner))))

      ((evaluated? $e)
       (elaborate (evaluated-ref $e)))

      ((typed? $t)
       (type-elaborate (typed-type $t) (typed-ref $t)))))

  ;; --- Checking Mode ---
  (define (type-elaborate $expected-type $term)
    (let* ([$elaborated (elaborate $term)]
           [$actual-type (get-type $elaborated)])
      (cond
        ((term=? $actual-type $expected-type)
         $elaborated)
        ((term=? $expected-type anything)
         $elaborated)
        (else
         (throw type-error "Type Mismatch"
                (list 'expected (term->datum $expected-type)
                      'got (term->datum $actual-type)))))))

  ;; --- Helpers ---

  (define (get-type $term)
    (switch $term
      ((typed? $t) (typed-type $t))
      ((type? $t)  (type (+ (type-depth $t) 1)))
      ((else _)    anything)))

  (define (untyped-internal $term)
    (switch $term
      ((typed? $t) (typed-ref $t))
      ((else _)    $term)))

  (define-rule-syntax (check-elaborates in out)
    (check-term->datum=? (elaborate in) out))
)
