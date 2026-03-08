(library (monadic)
  (export define-monadic)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (lets)
    (identifier)
    (switch)
    (commented))

  (commented
    (expects (id id-bind))
    (defines (id-map id-lets list->id append-id apply-id))
    (define-syntax (define-monadic $syntax)
      (syntax-case $syntax ()
        ((_ id)
          (identifier? #'id)
          (lets
            ($id-bind (identifier-append #'id #'id #'- #'bind))
            ($id-map (identifier-append #'id #'id #'- #'map))
            ($id-lets (identifier-append #'id #'id #'- #'lets))
            ($list->id (identifier-append #'id #'list #'-> #'id))
            ($append-id (identifier-append #'id #'append #'- #'id))
            ($apply-id (identifier-append #'id #'apply #'- #'id))
            #`(begin
              (define (#,$id-map x fn)
                (#,$id-bind x (lambda (v) (id (fn v)))))
              (define-rules-syntax
                ((#,$id-lets x) x)
                ((#,$id-lets (var expr) . x)
                  (identifier? #'var)
                  (#,$id-bind expr
                    (lambda (var)
                      (#,$id-lets . x)))))
              (define (#,$list->id $list)
                (switch $list
                  ((null? $null)
                    (id $null))
                  ((else $pair)
                    (#,$id-lets
                      ($car (car $pair))
                      ($cdr (#,$list->id (cdr $pair)))
                      (id (cons $car $cdr))))))
              (define (#,$append-id . x)
                (#,$list->id x))
              (define (#,$apply-id fn . x)
                (#,$id-lets
                  ($list (#,$list->id x))
                  (id (apply fn $list))))))))))
)
