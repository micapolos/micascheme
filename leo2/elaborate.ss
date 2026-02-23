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
      ((nothing? $nothing)
       (typed (type 0) $nothing))

      ((anything? $anything)
       (typed (type 0) $anything))

      ((type? $type)
       $type)

      ((native? $native)
       (throw elaborate $native))

      ((native-application? $native-application)
       (lets
         ($args (map elaborate (native-application-args $native-application)))
         (typed
          (typed (type 0) anything)
          (native-application
            (native-application-procedure $native-application)
            $args))))

      ((variable? $variable)
       (typed
         (typed (type 0) anything)
         $variable))

      ((procedure? $procedure)
       (throw elaborate $procedure))

      ((signature? $signature)
       (lets
         ($param (signature-param $signature))
         ($procedure (signature-procedure $signature))
         ($elab-param (elaborate $param))
         ($elab-proc (lambda ($v) (typed $elab-param $v)))
         ($sig-content (signature $elab-param (lambda ($v) (get-type ($elab-proc $v)))))
         (typed
           (typed (type 0) $sig-content)
           (signature $elab-param $elab-proc))))

      ((application? $application)
       (lets
         ($rhs (elaborate (application-rhs $application)))
         ($rhs-core (peel $rhs))
         ($rhs-type (get-type $rhs))
         ($lhs-raw (application-lhs $application))
         ($lhs
           (if (procedure? $lhs-raw)
               (elaborate (signature $rhs-type $lhs-raw))
               (elaborate $lhs-raw)))
         ($lhs-type (get-type $lhs))
         (switch (peel $lhs-type)
           ((signature? $sig)
            (lets
              ($res-type-term (signature-apply $sig $rhs-core))
              (typed $res-type-term (application $lhs $rhs))))
           ((else $other-type)
            (throw elaborate "Application LHS must have a signature type" $other-type)))))

      ((branch? $branch)
       (lets
         ($condition
           (type-elaborate
             (typed (type 0) (native 'Boolean))
             (branch-condition $branch)))
         ($consequent (elaborate (branch-consequent $branch)))
         ($alternate (elaborate (branch-alternate $branch)))
         ($consequent-type (get-type $consequent))
         ($alternate-type (get-type $alternate))
         (if (term=? (peel $consequent-type) (peel $alternate-type))
            (typed
              (smart-wrap $consequent-type)
              (branch $condition $consequent $alternate))
            (throw elaborate "Branch arm type mismatch" $consequent-type $alternate-type))))

      ((recursion? $recursion)
       (lets
         ($procedure (elaborate (recursion-procedure $recursion)))
         (typed
           (smart-wrap (get-type $procedure))
           (recursion (peel $procedure)))))

      ((labeled? $labeled)
       (lets
         ($inner (elaborate (labeled-ref $labeled)))
         (typed
           (smart-wrap (get-type $inner))
           (labeled (labeled-label $labeled) $inner))))

      ((evaluated? $evaluated)
        (elaborate (evaluated-ref $evaluated)))

      ((typed? $typed)
        $typed)))

  (define (smart-wrap $term)
    (switch $term
      ((type? $type) $type)
      ((typed? $typed) $typed)
      ((else $other) (typed (get-type $other) $other))))

  (define (type-elaborate $expected-type $term)
    (lets
      ($elaborated (elaborate $term))
      ($actual-type (get-type $elaborated))
      ($peeled-actual (peel $actual-type))
      ($peeled-expected (peel $expected-type))
      (cond
        ((term=? $peeled-actual $peeled-expected) $elaborated)
        ((term=? $peeled-expected anything) $elaborated)
        (else
          (throw elaborate "Type Mismatch"
            `(expected ,(term->datum $peeled-expected))
            `(actual ,(term->datum $peeled-actual)))))))

  (define (get-type $term)
    (switch $term
      ((typed? $typed)
        (typed-type $typed))
      ((type? $type)
        (type (+ (type-depth $type) 1)))
      ((else $other)
        (typed (type 0) anything))))

  (define (peel $term)
    (switch $term
      ((typed? $typed)
        (peel (typed-ref $typed)))
      ((labeled? $labeled)
        (peel (labeled-ref $labeled)))
      ((evaluated? $evaluated)
        (peel (evaluated-ref $evaluated)))
      ((else $other)
        $other)))

  (define-rule-syntax (check-elaborates in out)
    (check-term->datum=? (elaborate in) out))

  (define-rule-syntax (check-elaborate-throws in)
    (check (raises (elaborate in))))
)
