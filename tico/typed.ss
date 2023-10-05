(library (tico typed)
  (export
    typed typed? typed-thunk typed-type)
  (import
    (micascheme)
    (tico type)
    (tico thunk))

  (data (typed type thunk))

  (define (boolean->typed $boolean)
    (typed
      (boolean-type)
      (compile-time $boolean)))

  (define (number->typed $number)
    (typed
      (number-type)
      (compile-time $number)))

  (define (string->typed $string)
    (typed
      (string-type)
      (compile-time $string)))

  (define (boolean-type->typed)
    (typed
      (type-type)
      (compile-time (boolean-type))))

  (define (number-type->typed)
    (typed
      (type-type)
      (compile-time (number-type))))

  (define (string-type->typed)
    (typed
      (type-type)
      (compile-time (string-type))))

  (define (typed-struct $name $items.)
    (typed
      ())
)
