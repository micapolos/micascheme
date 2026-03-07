(library (leo2 normalize)
  (export
    normalize)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 typed-term))

  (define (normalize $term)
    (recurse-normalize normalize $term))

  (define (recurse-normalize $recurse $term)
    (switch-exhaustive $term
      ((lambda? $lambda)
        (lambda ($arg)
          ($recurse (lambda-apply $lambda $arg))))
      ((application? $application)
        (lets
          ($lhs ($recurse (application-lhs $application)))
          ($rhs ($recurse (application-rhs $application)))
          (switch $lhs
            ((native? $lhs-native)
              (switch $rhs
                ((native? $rhs-native)
                  (native-apply $lhs-native $rhs-native))
                ((else $rhs-other)
                  (application $lhs $rhs))))
            ((lambda? $lhs-lambda)
              ($recurse (lambda-apply $lhs-lambda $rhs)))
            ((lambda-type? $lhs-lambda-type)
              ($recurse (lambda-type-apply $lhs-lambda-type $rhs)))
            ((recursion? $lhs-recursion)
              ($recurse (application (recursion-apply $lhs-recursion $lhs) $rhs)))
            ((else _)
              (application $lhs $rhs)))))
      ((native? $native) $native)
      ((type? $type) $type)
      ((lambda-type? $lambda-type)
        (lambda-type
          ($recurse (lambda-type-param $lambda-type))
          ($recurse (lambda-type-lambda $lambda-type))))))

  (define (typed-normalize $typed)
    (recurse-typed-normalize typed-normalize $typed))

  (define (recurse-typed-normalize $recurse $typed)
    (type-value->term
      (term-type $typed)
      ($recurse (term-value $typed))))
)
