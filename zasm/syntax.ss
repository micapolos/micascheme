(library (zasm syntax)
  (export
    zasm-transform
    zasm-transform-expr)
  (import
    (micascheme)
    (syntax lookup)
    (zasm keywords))

  (define (zasm-transform $lookup $syntax)
    (syntax-case $syntax (org eq db dw begin)
      ((org expr)
        (lambda ($context-identifier)
          #`(context-org-set!
            #,$context-identifier
            #,(zasm-transform-expr $lookup #'expr))))
      ((db expr)
        (lambda ($context-identifier)
          #`(context-db! #,$context-identifier
            #,(zasm-transform-expr $lookup #'expr))))
      ((dw expr)
        (lambda ($context-identifier)
          #`(context-dw! #,$context-identifier
            #,(zasm-transform-expr $lookup #'expr))))
      ((begin op ...)
        (lambda ($context-identifier)
          #`(begin
            #,@(map-with ($op #'(op ...))
              ((zasm-transform $lookup $op) $context-identifier)))))))

  (define (zasm-transform-expr $lookup $syntax)
    (syntax-case $syntax ()
      (literal
        (literal? (datum literal))
        #'literal)))
)
