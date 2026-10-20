(import
  (scheme)
  (check)
  (arm64 verifier))

(let (($verifier (make-verifier #f)))
  (check (equal? (verifier-error? $verifier) #f))
  (check (raises (verifier-emit-a64 $verifier #x123)))
  (check (equal? (verifier-error? $verifier) #t)))
