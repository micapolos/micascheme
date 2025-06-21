(library (asm core)
  (export
    let*
    ;label db dw block local
    )
  (import
    (asm typed)
    ;(asm asm)
    (rename (micascheme) (let* %let*)))

  (define-typed (let* $lookup $syntax)
    (syntax-case $syntax ()
      ((_ () body)
        (syntax->typed $lookup #'body))
      ((_ (entry entry* ...) body)
        (syntax->typed $lookup
          #'(let (entry) (let* (entry* ...) body))))))

  ; (define-asm (db $lookup $syntax)
  ;   (syntax-case $syntax ()
  ;     ((_ expr ...)
  ;       (lambda ($block)
  ;         (block+binary-syntax-proc $block 1
  ;           (lambda ($org) #'(db-binary expr ...)))))))

  ; (define-asm (dw $lookup $syntax)
  ;   (syntax-case $syntax ()
  ;     ((_ expr ...)
  ;       (lambda ($block)
  ;         (block+binary-syntax-proc $block 2
  ;           (lambda ($org) #'(dw-binary expr ...)))))))

  ; (define-asm (label $lookup $syntax)
  ;   (syntax-case $syntax ()
  ;     ((_ id)
  ;       (lambda ($block)
  ;         (block+label $block #'id)))))

  ; (define-asm (block $lookup $syntax)
  ;   (syntax-case $syntax ()
  ;     ((_ asm ...)
  ;       (lets
  ;         ($asm (syntaxes->asm $lookup #'(asm ...)))
  ;         (lambda ($block) ($asm $block))))))

  ; (define-asm (local $lookup $syntax)
  ;   (syntax-case $syntax ()
  ;     ((_ asm ...)
  ;       (lets
  ;         ($asm (syntaxes->asm $lookup #'(asm ...)))
  ;         (lambda ($block)
  ;           (block+local $block ($asm (empty-block))))))))
)
