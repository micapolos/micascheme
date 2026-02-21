(library (leo2 code)
  (export
    atom->code
    phrase->code
    check-atom->code=?
    check-phrase->code=?)
  (import
    (leo2 base)
    (code))

  (define (atom->code $atom)
    (string-code
      (format "~s" $atom)))

  (define (phrase->code $phrase)
    (list->separated-code
      (code " ")
      (map atom->code $phrase)))

  (define-rule-syntax (check-atom->code=? in out)
    (check-code=? (atom->code in) out))

  (define-rule-syntax (check-phrase->code=? in out)
    (check-code=? (phrase->code in) out))
)
