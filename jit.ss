(library (jit)
  (export
    jit?
    jit-address
    jit-size

    (rename (%jit jit))
    jit-edit!
    jit-exec!)
  (import
    (scheme)
    (data)
    (mman)
    (lets)
    (scoped))

  (data (jit address size))

  (define-scoped %jit
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((var (size) body)
          #'(lets
            (size-var size)
            (address
              (mmap
                0 ; address
                size-var
                (fxior PROT_READ PROT_WRITE)
                (fxior MAP_PRIVATE MAP_ANONYMOUS)
                -1 ; fd, must be -1
                0 ; offset, must be 0
                ))
            (if (= address MAP_FAILED)
              (error 'jit "mmap failed")
              (lets
                (var (jit address size-var))
                (dynamic-wind
                  (lambda () #f)
                  (lambda () body)
                  (lambda () (munmap address size-var))))))))))

  (define (jit-edit! $jit)
    (unless
      (zero? (mprotect (jit-address $jit) (jit-size $jit) (fxior PROT_READ PROT_WRITE)))
      (error 'jit-edit! "mprotect failed")))

  (define (jit-exec! $jit)
    (unless
      (zero? (mprotect (jit-address $jit) (jit-size $jit) (fxior PROT_READ PROT_EXEC)))
      (error 'jit-exec! "mprotect failed")))
)
