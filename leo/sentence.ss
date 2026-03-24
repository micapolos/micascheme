(library (leo sentence)
  (export
    sentence-pretty?
    primitive-word
    sentence sentence? sentence-word sentence-args
    ->sentence?)
  (import
    (micascheme)
    (leo word))

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
          (list (char->datum $char))))
      ((pair? $pair)
        (switch? (car $pair)
          ((symbol? $symbol)
            (sentence
              (symbol->word $symbol)
              (cdr $pair)))))
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
