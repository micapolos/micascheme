(library (zexy-2 syntax)
  (export
    syntax->value
    syntax->u8
    syntax->u16
    c-syntax->u3)
  (import
    (micascheme)
    (syntax lookup)
    (zexy-2 value)
    (zexy-2 keywords))

  (define (syntax->value $lookup $syntax)
    (syntax-case $syntax ()
      (x
        (identifier? #'x)
        (lookup-ref $lookup #'x))
      (other
        (datum other))))

  (define (syntax->u8 $lookup $syntax)
    (lets
      ($value (syntax->value $lookup $syntax))
      (if (u8? $value)
        $value
        (syntax-error $syntax "invalid byte"))))

  (define (syntax->u16 $lookup $syntax)
    (lets
      ($value (syntax->value $lookup $syntax))
      (if (u16? $value)
        $value
        (syntax-error $syntax "invalid word"))))

  (define (c-syntax->u3 $syntax)
    (syntax-case $syntax (nz z nc c po pe p m)
      (nz #b000)
      (z  #b001)
      (nc #b010)
      (c  #b011)
      (po #b100)
      (pe #b101)
      (p  #b110)
      (p  #b111)))
)
