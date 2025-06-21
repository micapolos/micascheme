(library (asm-2 core)
  (export let*)
  (import
    (asm-2 typed)
    (rename (micascheme)
      (let* %let*)))

  (define-typed (let* $lookup $syntax)
    (syntax-case $syntax ()
      ((_ () body)
        (syntax->typed $lookup #'body))
      ((_ (entry entry* ...) body)
        (syntax->typed $lookup
          #'(let (entry) (let* (entry* ...) body))))))
)
