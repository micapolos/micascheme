(library (micalang term)
  (export
    a-type a-type?
    a-symbol a-symbol?
    a-boolean a-boolean?
    a-number a-number?
    a-char a-char?
    a-string a-string?
    native native? native-ref
    variable variable? variable-index
    constant constant? constant-ref
    tagged tagged? tagged-tag tagged-ref
    abstraction abstraction? abstraction-symbol? abstraction-param abstraction-procedure abstraction-apply
    application application? application-lhs application-rhs
    type-abstraction type-abstraction? type-abstraction-symbol? type-abstraction-param type-abstraction-procedure type-abstraction-apply
    conditional conditional? conditional-cond conditional-true conditional-false
    macro macro? macro-procedure macro-apply

    term-apply
    apply-term
    default-depth-term=? term=?)
  (import (micalang base))

  (data a-type)
  (data a-symbol)
  (data a-boolean)
  (data a-number)
  (data a-char)
  (data a-string)
  (data (native ref))
  (data (variable index))
  (data (constant ref))
  (data (tagged tag ref))
  (data (abstraction symbol? param procedure))
  (data (application lhs rhs))
  (data (type-abstraction symbol? param procedure))
  (data (conditional cond true false))
  (data (macro procedure))

  (define (apply-term $procedure $rhs)
    (switch $rhs
      ((native? $native)
        (native ($procedure (native-ref $native))))
      ((tagged? $tagged)
        (apply-term $procedure (tagged-ref $tagged)))
      ((else $other)
        (application (native $procedure) $other))))

  (define (term-apply $lhs $rhs)
    (switch $lhs
      ((native? $native)
        (apply-term (native-ref $native) $rhs))
      ((tagged? $tagged)
        (term-apply (tagged-ref $tagged) $rhs))
      ((abstraction? $abstraction)
        (abstraction-apply $abstraction $rhs))
      ((type-abstraction? $type-abstraction)
        (type-abstraction-apply $type-abstraction $rhs))
      ((else $other)
        (application $other $rhs))))

  (define (term=? $lhs $rhs)
    (default-depth-term=?
      (lambda ($default $depth $lhs $rhs) (throw term=? $lhs $rhs))
      0
      $lhs
      $rhs))

  (define (default-depth-term=? $default $depth $lhs $rhs)
    (switch $lhs
      ((a-type? _)
        (switch? $rhs
          ((a-type? _) #t)))
      ((a-symbol? _)
        (switch? $rhs
          ((a-symbol? _) #t)))
      ((a-boolean? _)
        (switch? $rhs
          ((a-boolean? _) #t)))
      ((a-number? _)
        (switch? $rhs
          ((a-number? _) #t)))
      ((a-char? _)
        (switch? $rhs
          ((a-char? _) #t)))
      ((a-string? _)
        (switch? $rhs
          ((a-string? _) #t)))
      ((native? $lhs-native)
        (switch? $rhs
          ((native? $rhs-native)
            (equal?
              (native-ref $lhs-native)
              (native-ref $rhs-native)))))
      ((variable? $lhs-variable)
        (switch? $rhs
          ((variable? $rhs-variable)
            (=
              (variable-index $lhs-variable)
              (variable-index $rhs-variable)))))
      ((constant? $lhs-constant)
        (switch? $rhs
          ((constant? $rhs-constant)
            (equal?
              (constant-ref $lhs-constant)
              (constant-ref $rhs-constant)))))
      ((tagged? $lhs-tagged)
        (switch? $rhs
          ((tagged? $rhs-tagged)
            (and
              (default-depth-term=? $default $depth
                (tagged-tag $lhs-tagged)
                (tagged-tag $rhs-tagged))
              (default-depth-term=? $default $depth
                (tagged-ref $lhs-tagged)
                (tagged-ref $rhs-tagged))))))
      ((abstraction? $lhs-abstraction)
        (switch? $rhs
          ((abstraction? $rhs-abstraction)
            (and
              (default-depth-term=? $default $depth
                (abstraction-param $lhs-abstraction)
                (abstraction-param $rhs-abstraction))
              (default-depth-term=? $default (+ $depth 1)
                (abstraction-apply $lhs-abstraction (variable $depth))
                (abstraction-apply $rhs-abstraction (variable $depth)))))))
      ((application? $lhs-application)
        (switch? $rhs
          ((application? $rhs-application)
            (and
              (default-depth-term=? $default $depth
                (application-lhs $lhs-application)
                (application-lhs $rhs-application))
              (default-depth-term=? $default $depth
                (application-rhs $lhs-application)
                (application-rhs $rhs-application))))))
      ((type-abstraction? $lhs-pi)
        (switch? $rhs
          ((type-abstraction? $rhs-pi)
            (and
              (default-depth-term=? $default $depth
                (type-abstraction-param $lhs-pi)
                (type-abstraction-param $rhs-pi))
              (default-depth-term=? $default (+ $depth 1)
                (type-abstraction-apply $lhs-pi (variable $depth))
                (type-abstraction-apply $rhs-pi (variable $depth)))))))
      ((conditional? $lhs-conditional)
        (switch? $rhs
          ((conditional? $rhs-conditional)
            (and
              (default-depth-term=? $default $depth
                (conditional-cond $lhs-conditional)
                (conditional-cond $rhs-conditional))
              (default-depth-term=? $default $depth
                (conditional-true $lhs-conditional)
                (conditional-true $rhs-conditional))
              (default-depth-term=? $default $depth
                (conditional-false $lhs-conditional)
                (conditional-false $rhs-conditional))))))
      ((macro? $lhs-macro)
        (switch? $rhs
          ((macro? $rhs-macro)
            (eq?
              (macro-procedure $lhs-macro)
              (macro-procedure $rhs-macro)))))
      ((else $other)
        ($default $default $depth $lhs $rhs))))

  (define (abstraction-apply $abstraction $arg)
    ((abstraction-procedure $abstraction) $arg))

  (define (type-abstraction-apply $type-abstraction $arg)
    ((type-abstraction-procedure $type-abstraction) $arg))

  (define (macro-apply $macro $compiler $term)
    ((macro-procedure $macro) $compiler $term))
)
