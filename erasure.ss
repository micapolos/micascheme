(library (erasure)
  (export erase)
  (import 
    (micascheme)
    (prefix (typed) t-)
    (term))

  (define (erase $typed)
    (switch $typed
      ((number? $number) $number)
      ((string? $string) $string)
      ((t-typed? $typed) 
        (erase (t-typed-value $typed)))
      ((t-native? $native)
        (native (t-native-value $native)))
      ((t-variable? $variable) 
        (variable (t-variable-index $variable)))
      ((t-function? $function) 
        (function
          (length (t-function-params $function))
          (erase (t-function-body $function))))
      ((t-application? $application)
        (application
          (erase (t-application-fn $application))
          (map erase (t-application-args $application))))
      ((t-tuple? $tuple)
        (tuple 
          (map erase (t-tuple-items $tuple))))))
)