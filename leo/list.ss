(library (leo list)
  (export null list open-list closed-list)
  (import
    (rename (scheme) (list %list))
    (syntaxes))

  (define null '())
  (define closed-list %list)
  (define open-list list*)

  (define-rules-syntax
    (keywords and)

    ((list xs ... (and last))
      (open-list xs ... last))
    ((list xs ...)
      (closed-list xs ...)))
)
