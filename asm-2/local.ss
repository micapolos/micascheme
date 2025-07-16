(library (asm-2 local)
  (export
    local local? local-proc local-ref
    local-apply
    local-with)
  (import
    (micascheme)
    (syntax lookup))

  (data (local proc ref))

  (define (local-apply $local $lookup)
    ((local-proc $local) $lookup))

  (define-rule-syntax (local-with (key value) ... body)
    (local
      (lambda ($lookup)
        (fold-left
          (partial lookup+ $lookup)
          $lookup
          (list #'key ...)
          (list value ...)))
      body))
)
