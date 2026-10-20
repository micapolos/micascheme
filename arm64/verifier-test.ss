(import
  (scheme)
  (check)
  (lets)
  (arm64 verifier))

(lets
  ($verifier (make-verifier #f))
  (begin
    (check (equal? (verifier-error? $verifier) #f))
    (check (raises (verifier-emit-a64 $verifier)))

    ;(check (equal? (verifier-error? $verifier) #t))
    ))
