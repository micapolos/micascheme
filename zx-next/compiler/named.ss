(library (zx-next compiler named)
  (export
    named
    check-named)
  (import
    (micascheme)
    (prefix (zx-next compiler named-keywords) %)
    (prefix (zx-next compiler indexed) %%))

  (define (named $lookup $syntax)
    (syntax-case $syntax (%%lets)
      ((args locals size (%native x)) #'x)
      ((args locals size id)
        (identifier? #'id)
        (or
          (local? #'id #'locals)
          (arg? #'id 0 #'args)))
      ((args locals size (%lets (id var-size expr) ... body))
        (lets
          ($nameds
            (map list
              #'(id ...)
              #'(var-size ...)
              (map (partial named $lookup) #'((args locals var-size expr) ...))))
          (syntax-case $nameds ()
            (((id var-size expr) ...)
              #`(%%lets (var-size expr) ...
                #,(named $lookup #'(args (id ... . locals) size body)))))))
      ((args locals size n)
        (integer? (datum n))
        #`(%%const n))))

  (define (local? $id $locals)
    (syntax-case? $locals ()
      ((local . locals)
        (if (free-identifier=? #'local $id)
          #`(%%local #,(length (syntax->list #'locals)))
          (local? $id #'locals)))))

  (define (arg? $id $index $args)
    (syntax-case? $args ()
      ((arg . args)
        (if (free-identifier=? #'arg $id)
          #`(%%arg #,$index)
          (arg? $id (+ $index 1) #'args)))))

  (define-rule-syntax (check-named lookup in out)
    (check (equal? (syntax->datum (named lookup #'in)) 'out)))
)
