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
    (leo2 datum)
    (leo2 stdlib))

  (define (elaborate $term)
    (term-switch $term
      ((hole? $hole)
        (typed (type 0) $hole))

      ((nothing? $nothing)
        (typed (type 0) $nothing))

      ((type? $type)
        $type)

      ((native? $native)
        (throw elaborate
          `(unsupported-native ,(native-ref $native))))

      ((native-application? $native-application)
        (lets
          ($args (map elaborate (native-application-args $native-application)))
          (typed
            (typed (type 0) (hole 0))
            (native-application
              (native-application-lambda $native-application)
              $args))))

      ((variable? $variable)
        (typed
          (typed (type 0) (hole 0))
          $variable))

      ((lambda? $lambda)
        (throw elaborate (list 'raw-lambda-unsupported (term->datum $lambda))))

      ((lambda-type? $lambda-type)
        (lets
          ($param (lambda-type-param $lambda-type))
          ($lambda (lambda-type-lambda $lambda-type))
          ($typed-param (elaborate $param))
          ($typed-proc (lambda ($v) (typed $typed-param $v)))
          ($lambda-type-type
            (lambda-type $typed-param
              (lambda ($v)
                (term-type ($typed-proc $v)))))
          (typed
            (typed (type 0) $lambda-type-type)
            (lambda-type $typed-param $typed-proc))))

      ((application? $application)
        (lets
          ($rhs (elaborate (application-rhs $application)))
          ($rhs-core (term-core $rhs))
          ($rhs-type (term-type $rhs))
          ($lhs (application-lhs $application))
          ($typed-lhs
            (if (lambda? $lhs)
              (elaborate (lambda-type $rhs-type $lhs))
              (elaborate $lhs)))
          ($lhs-type (term-type $typed-lhs))
          (switch (term-core $lhs-type)
            ((lambda-type? $lambda-type)
              (lets
                ($res-type-term (lambda-type-apply $lambda-type $rhs-core))
                (typed $res-type-term (application $typed-lhs $rhs))))
            ((else $other-type)
              (throw elaborate
                `(invalid-application-lhs
                  ,(term->datum $other-type)))))))

      ((branch? $branch)
        (lets
          ($condition
            (type-elaborate
              (typed (type 0) (native boolean-type))
              (branch-condition $branch)))
          ($consequent (elaborate (branch-consequent $branch)))
          ($alternate (elaborate (branch-alternate $branch)))
          ($consequent-type (term-type $consequent))
          ($alternate-type (term-type $alternate))
          (typed
            (branch
              (term-core $condition)
              (term-core $consequent-type)
              (term-core $alternate-type))
            (branch $condition $consequent $alternate))))

      ((recursion? $recursion)
        (lets
          ($typed-lambda (elaborate (recursion-lambda $recursion)))
          (typed
            (term->typed (term-type $typed-lambda))
            (recursion (term-core $typed-lambda)))))

      ((labeled? $labeled)
        (lets
          ($typed-ref (elaborate (labeled-ref $labeled)))
          (typed
            (term->typed (term-type $typed-ref))
            (labeled (labeled-label $labeled) $typed-ref))))

      ((evaluated? $evaluated)
        (elaborate (evaluated-ref $evaluated)))

      ((typed? $typed)
        $typed)))

  (define (term->typed $term)
    (switch $term
      ((type? $type) $type)
      ((typed? $typed) $typed)
      ((else $other) (typed (term-type $other) $other))))

  (define (type-elaborate $expected-type $term)
    (lets
      ($elaborated (elaborate $term))
      ($actual-type (term-type $elaborated))
      ($actual-core (term-core $actual-type))
      ($expected-core (term-core $expected-type))
      (cond
        ((term=? $actual-core $expected-core) $elaborated)
        ((term=? $expected-core (hole 0)) $elaborated)
        (else
          (throw elaborate
            `(type-mismatch
              (expected ,(term->datum $expected-core))
              (actual ,(term->datum $actual-core))))))))

  (define (term-type $term)
    (switch $term
      ((typed? $typed)
        (typed-type $typed))
      ((type? $type)
        (type (+ (type-depth $type) 1)))
      ((else $other)
        (typed (type 0) (hole 0)))))

  (define (term-core $term)
    (switch $term
      ((typed? $typed)
        (term-core (typed-ref $typed)))
      ((labeled? $labeled)
        (term-core (labeled-ref $labeled)))
      ((evaluated? $evaluated)
        (term-core (evaluated-ref $evaluated)))
      ((else $other)
        $other)))

  (define-rule-syntax (check-elaborates in out)
    (check-term->datum=? (elaborate in) out))

  (define-rule-syntax (check-elaborate-throws in)
    (check (raises (elaborate in))))
)
