(library (leo2 annotation)
  (export labeled-annotation-ref?)
  (import
    (leo2 base)
    (leo2 term))

  (comment
    (annotation
      (is labeled term)
      (with quoted pair)
      (where car contains identifier symbol)
      (and cadr contains annotation value))
    (example
      (labeled (quoted '(annotation-id "this is annotation value")))))

  (define (labeled-annotation-ref? $labeled $symbol)
    (switch? (labeled-label $labeled)
      ((quoted? $quoted)
        (lets
          ($ref (quoted-ref $quoted))
          (and
            (pair? $ref)
            (eq? (car $ref) $symbol)
            (pair? (cdr $ref))
            (cadr $ref))))))
)
