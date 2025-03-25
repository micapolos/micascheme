(library (typed-scheme lang)
  (export define-type)
  (import
    (micascheme)
    (typed-scheme type))

  (define-syntax (define-type $syntax)
    (syntax-case $syntax ()
      ((_ name)
        #`(define-type name 0))
      ((_ name arity)
        (lets
          ($name (syntax->datum (identifier name)))
          ($arity (syntax->datum #'arity))
          (run (unless (nonnegative-integer? $arity)
            (syntax-error $syntax "invalid arity")))
          ($gensym (gensym))
          ($type-definition #`(type-definition #,$gensym '#,$name #,$arity))
          ($params (generate-temporaries (iota $arity)))
          (case $arity
            ((0)
              #`(define name
                (defined-type #,$type-definition (list))))
            (else
              #`(define (name #,@$params)
                (defined-type #,$type-definition #,@$params))))))))
)
