(library (leo2 elaborate)
  (export
    elaborate
    type-elaborate
    check-elaborates
    check-elaborate-throws)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 equal)
    (leo2 datum))

  (define (elaborate $term)
    (term-switch $term
      ((nothing? _)
       (typed (type 0) nothing))

      ((anything? _)
       (typed (type 0) anything))

      ((type? $type)
       $type)

      ((native? $native)
       (typed (typed (type 0) anything) $native))

      ((native-application? $na)
       (let ([$args (map elaborate (native-application-args $na))])
         (typed (typed (type 0) anything)
                (native-application (native-application-procedure $na) $args))))

      ((variable? $v)
       (switch $v
         ((typed? _) $v)
         ((else _) (typed (typed (type 0) anything) $v))))

      ((procedure? $proc)
       (throw type-error "Cannot infer signature for raw procedure" $proc))

      ((signature? $sig)
       (let* ([$param (signature-param $sig)]
              [$proc  (signature-procedure $sig)]
              [$elab-param (elaborate $param)]
              [$elab-proc
               (lambda ($v) (elaborate ($proc $v)))])
         (let ([$sig-type (signature $elab-param (lambda ($v) (type 0)))])
           ;; We wrap signature types in Type 0, but use a local wrapper
           ;; to avoid re-elaborating the whole signature.
           (typed (typed (type 0) $sig-type)
                  (signature $elab-param $elab-proc)))))

      ((application? $app)
       (let* ([$rhs (elaborate (application-rhs $app))]
              [$rhs-core (peel $rhs)]
              [$rhs-type (get-type $rhs)])
         (let ([$lhs-raw (application-lhs $app)])
           (let ([$lhs (if (procedure? $lhs-raw)
                           (elaborate (signature $rhs-type $lhs-raw))
                           (elaborate $lhs-raw))])
             (let ([$lhs-type (get-type $lhs)])
               (switch (peel $lhs-type)
                 ((signature? $sig)
                  (let ([$res-type (signature-apply $sig $rhs-core)])
                    ;; LOOP FIX: Don't call elaborate here.
                    ;; Use a smart-wrap that respects the Bedrock rule.
                    (typed (smart-wrap $res-type) (application $lhs $rhs))))
                 ((else _)
                  (throw type-error "Application LHS must have a signature type" $lhs-type))))))))

      ((branch? $branch)
       (let* ([$cond (type-elaborate (typed (type 0) (native 'Boolean)) (branch-condition $branch))]
              [$then (elaborate (branch-consequent $branch))]
              [$else (elaborate (branch-alternate $branch))]
              [$then-type (get-type $then)]
              [$else-type (get-type $else)])
         (if (term=? (peel $then-type) (peel $else-type))
             (typed (smart-wrap $then-type) (branch $cond $then $else))
             (throw type-error "Branch arm type mismatch" (list $then-type $else-type)))))

      ((recursion? $rec)
       (let ([$proc (elaborate (recursion-procedure $rec))])
         (typed (smart-wrap (get-type $proc)) (recursion (peel $proc)))))

      ((labeled? $l)
       (let ([$inner (elaborate (labeled-ref $l))])
         (typed (smart-wrap (get-type $inner)) (labeled (labeled-label $l) $inner))))

      ((evaluated? $e)
       (elaborate (evaluated-ref $e)))

      ((typed? $t)
       (type-elaborate (typed-type $t) (typed-ref $t)))))

  ;; --- Smart Wrap to prevent infinite recursion ---
  (define (smart-wrap $term)
    (switch $term
      ((type? _) $term) ;; Universes are already bedrock
      ((typed? _) $term) ;; Already wrapped
      ((else _) (typed (get-type $term) $term))))

  (define (type-elaborate $expected-type $term)
    (let* ([$elaborated (elaborate $term)]
           [$actual-type (get-type $elaborated)]
           [$peeled-actual (peel $actual-type)]
           [$peeled-expected (peel $expected-type)])
      (cond
        ((term=? $peeled-actual $peeled-expected)
         $elaborated)
        ((term=? $peeled-expected anything)
         $elaborated)
        (else
         (throw type-error "Type Mismatch"
                (list 'expected (term->datum $peeled-expected)
                      'got (term->datum $peeled-actual)))))))

  (define (get-type $term)
    (switch $term
      ((typed? $t) (typed-type $t))
      ((type? $t)  (type (+ (type-depth $t) 1)))
      ((else _)    (typed (type 0) anything))))

  (define (peel $term)
    (switch $term
      ((typed? $t)     (peel (typed-ref $t)))
      ((labeled? $l)   (peel (labeled-ref $l)))
      ((evaluated? $e) (peel (evaluated-ref $e)))
      ((else _)        $term)))

  (define (untyped-internal $term)
    (switch $term
      ((typed? $t) (typed-ref $t))
      ((else _)    $term)))

  (define-rule-syntax (check-elaborates in out)
    (check-term->datum=? (elaborate in) out))

  (define-rule-syntax (check-elaborate-throws in)
    (check (raises (elaborate in))))
)
