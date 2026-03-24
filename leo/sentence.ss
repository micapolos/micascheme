(library (leo sentence)
  (export
    sentence-pretty?
    primitive-word
    sentence sentence? sentence-word sentence-args
    ->sentence?)
  (import (micascheme))

  (data (sentence word args))

  (define sentence-pretty? (make-thread-parameter #f))

  (define (primitive-word $string)
    (cond
      ((sentence-pretty?) $string)
      (else (string-append "#" $string))))

  (define (->sentence? $obj)
    (switch? $obj
      ((char? $char)
        (sentence
          (primitive-word "char")
          (lets
            ($string (format "~s" $char))
            (list
              (string->symbol
                (substring $string 2
                  (string-length $string)))))))
      ((pair? $pair)
        (switch? (car $pair)
          ((symbol? $symbol)
            (switch? (cdr $pair)
              ((singleton-list? $cdr)
                (sentence
                  (symbol->string $symbol)
                  (car $cdr)))))))
      ((box? $box)
        (sentence
          (primitive-word "box")
          (list (unbox $box))))
      ((bytevector? $bytevector)
        (sentence
          (primitive-word "bytevector")
          (bytevector->u8-list $bytevector)))
      ((vector? $vector)
        (sentence
          (primitive-word "vector")
          (vector->list $vector)))))
)
