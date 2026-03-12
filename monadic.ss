(library (monadic)
  (export define-monadic)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (lets)
    (identifier)
    (switch)
    (commented)
    (keyword))

  (commented
    (expects (id id-bind))
    (defines (id-map id-lets id-switch list->id append-id apply-id))
    (define-syntax (define-monadic $syntax)
      (syntax-case $syntax ()
        ((_ id)
          (keyword? id)
          (lets
            ($id-bind (keyword-append id id - bind))
            ($id-map (keyword-append id id - map))
            ($id-lets (keyword-append id id - lets))
            ($id-switch (keyword-append id id - switch))
            ($list->id (keyword-append id list -> id))
            ($append-id (keyword-append id append - id))
            ($apply-id (keyword-append id apply - id))
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
              (define-rules-syntax
                ((#,$id-switch expr case (... ...))
                  (#,$id-lets
                    (id expr)
                    (switch id case (... ...)))))
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
