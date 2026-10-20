(import
  (scheme)
  (check)
  (arm64 verifier))

(let (($verifier (make-verifier)))
  (check (verifier-state-start? (verifier-state $verifier)))
  (check (raises (verifier-emit-a64 $verifier #x123)))
  (check (verifier-state-error? (verifier-state $verifier))))
