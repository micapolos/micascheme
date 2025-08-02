(library (zx-next scheme compiler)
  (export
    byte word
    byte+ byte-
    write
    syntax->expr
    check-syntax->expr)
  (import
    (except (micascheme) write)
    (prefix (zx-next scheme primitives) %))

  (define-keywords byte word byte+ byte- write)

  (define (syntax->expr $lookup $syntax)
    (syntax-case $syntax (byte word byte+ byte- write)
      ((byte n)
        #`((%push-byte n)))
      ((word n)
        #`((%push-word n)))
      ((byte+ a b)
        #`(
          #,@(syntax->expr $lookup #'b)
          #,@(syntax->expr $lookup #'a)
          (%byte-add)))
      ((byte- a b)
        #`(
          #,@(syntax->expr $lookup #'b)
          #,@(syntax->expr $lookup #'a)
          (%byte-sub)))
      ((write x)
        #`(
          #,@(syntax->expr $lookup #'x)
          (%write-value)))))

  (define-rule-syntax (check-syntax->expr lookup x op ...)
    (check (equal? (syntax->datum (syntax->expr lookup #'x)) '(op ...))))
)
