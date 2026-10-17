(library (emit)
  (export
    emit-u16-le
    emit-u32-le
    check-emits)
  (import
    (scheme)
    (core)
    (syntax)
    (lets)
    (check))

  ; Assumes emit-u8 is implicit

  (define-syntax (emit-u16-le $syntax)
    (syntax-case $syntax ()
      ((id expr)
        (with-implicit (id emit-u8)
          #`(let ((u16-le expr))
            (emit-u8 (fxand u16-le #xff))
            (emit-u8 (fxsrl u16-le 8)))))))

  (define-syntax (emit-u32-le $syntax)
    (syntax-case $syntax ()
      ((id expr)
        (with-implicit (id emit-u16-le)
          #`(let ((u32-le expr))
            (emit-u16-le (fxand u32-le #xffff))
            (emit-u16-le (fxsrl u32-le 16)))))))

  (define-syntax (check-emits $syntax)
    (syntax-case $syntax ()
      ((id xs ... (u8s ...))
        (with-implicit (id emit-u8)
          #`(let* ((emitted '()) (emit-u8 (lambda (x) (cons! x emitted))))
            xs ...
            (check (equal? (reverse emitted) '(u8s ...))))))))
)
