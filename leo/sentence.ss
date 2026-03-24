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
          (list (char->datum $char))))
      ((pair? $pair)
        (switch? (car $pair)
          ((symbol? $symbol)
            (sentence-resolve-quotes
              (sentence
                (symbol->string $symbol)
                (cdr $pair))))))
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

  (define (sentence-resolve-quotes $sentence)
    (or
      (switch? (sentence-args $sentence)
        ((singleton-list? $args)
          (switch? (->sentence? (car $args))
            ((sentence? $args-sentence)
              (case (sentence-word $sentence)
                (("quasiquote")
                  (sentence-resolve-quotes
                    (sentence
                      (string-append "'" (sentence-word $args-sentence))
                      (sentence-args $args-sentence))))
                (else
                  (case (sentence-word $args-sentence)
                    (("unquote")
                      (sentence-resolve-quotes
                        (sentence
                          (string-append (sentence-word $sentence) "'")
                          (sentence-args $args-sentence))))
                    (("unquote-splicing")
                      (sentence-resolve-quotes
                        (sentence
                          (string-append (sentence-word $sentence) "'...")
                          (sentence-args $args-sentence))))
                    (else #f))))))))
        $sentence))
)
