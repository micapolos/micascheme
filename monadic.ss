(library (monadic)
  (export define-monadic)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (lets)
    (identifier)
    (switch))

  (define-syntax (define-monadic $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (identifier? #'id)
        (lets
          ($bind-id (identifier-append #'id #'id #'- #'bind))
          ($lets-id (identifier-append #'id #'id #'- #'lets))
          ($list->id (identifier-append #'id #'list #'-> #'id))
          ($append-id (identifier-append #'id #'append #'- #'id))
          #`(begin
            (define-rules-syntax
              ((#,$lets-id x) x)
              ((#,$lets-id (var expr) . x)
                (identifier? #'var)
                (#,$bind-id expr
                  (lambda (var)
                    (#,$lets-id . x)))))
            (define (#,$list->id $list)
              (switch $list
                ((null? $null)
                  (id $null))
                ((else $pair)
                  (#,$lets-id
                    ($car (car $pair))
                    ($cdr (#,$list->id (cdr $pair)))
                    (id (cons $car $cdr))))))
            (define (#,$append-id . x)
              (#,$list->id x)))))))
)
