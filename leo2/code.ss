(library (leo2 code)
  (export
    datum->code
    check-datum->code=?)
  (import
    (leo2 base)
    (code))

  (define (datum->code $datum)
    (string-code (format "~s" $datum)))

  (define-rule-syntax (check-datum->code=? in out)
    (check-code=? (datum->code in) out))
)
