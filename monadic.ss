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
    (expects (id id-let1))
    (defines (id-map id-lets id-lets? id-switch list->id replace-id id-append apply-id id-or id-and))
    (define-syntax (define-monadic $syntax)
      (syntax-case $syntax ()
        ((_ id)
          (keyword? id)
          (lets
            ($id-let1 (keyword-append id id - let1))
            ($id-map (keyword-append id id - map))
            ($id-lets (keyword-append id id - lets))
            ($id-lets? (keyword-append id id - lets?))
            ($id-switch (keyword-append id id - switch))
            ($list->id (keyword-append id list -> id))
            ($replace-id (keyword-append id replace - id))
            ($id-append (keyword-append id id - append))
            ($apply-id (keyword-append id apply - id))
            ($id-or (keyword-append id id - or))
            ($id-and (keyword-append id id - and))
            #`(begin
              (define-rules-syntax
                ((#,$id-lets x) x)
                ((#,$id-lets (var expr) . x)
                  (identifier? #'var)
                  (#,$id-let1
                    (var expr)
                    (#,$id-lets . x))))
              (define (#,$id-map x fn)
                (#,$id-let1 (v x) (id (fn v))))
              (define-rules-syntax
                ((#,$id-lets? x) x)
                ((#,$id-lets? (var expr) . x)
                  (#,$id-lets
                    (var expr)
                    (if var
                      (#,$id-lets? . x)
                      (id #f)))))
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
              (define (#,$replace-id x v)
                (#,$id-map x
                  (lambda (_) v)))
              (define (#,$id-append . x)
                (#,$list->id x))
              (define (#,$apply-id fn . x)
                (#,$id-lets
                  ($list (#,$list->id x))
                  (id (apply fn $list))))
              (define-rules-syntax
                ((#,$id-or) (id #f))
                ((#,$id-or x xs (... ...))
                  (#,$id-lets
                    ($value? x)
                    (if $value?
                      (id $value?)
                      (#,$id-or xs (... ...))))))
              (define-rules-syntax
                ((#,$id-and) (id #t))
                ((#,$id-and x) x)
                ((#,$id-and x xs (... ...))
                  (#,$id-lets
                    ($value? x)
                    (if $value?
                      (#,$id-and xs (... ...))
                      (id #f)))))))))))
)
