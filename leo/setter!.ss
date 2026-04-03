(library (leo setter!)
  (export setter!)
  (import
    (scheme)
    (identifier))

  (define-syntax (setter! $syntax)
    (syntax-case $syntax ()
      ((_ (target id))
        (identifier-append #'target #'target #'- #'id #'- #'set!))))
)
