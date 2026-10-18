(library (jit)
  (export
    (rename (%jit jit))
    jit?
    jit-address
    jit-size
    jit-edit
    jit-exec
    jit-destroy

    with-jit)
  (import
    (scheme)
    (data)
    (mman)
    (lets)
    (scoped))

  (data (jit address size))

  (define (%jit $size)
    (lets
      ($address
        (mmap
          0 ; address
          $size
          (fxior PROT_READ PROT_WRITE)
          (fxior MAP_PRIVATE MAP_ANONYMOUS)
          -1 ; fd, must be -1
          0 ; offset, must be 0
          ))
      (if (= $address MAP_FAILED)
        (error 'jit "mmap failed")
        (jit $address $size))))

  (define (jit-edit $jit)
    (unless
      (zero? (mprotect (jit-address $jit) (jit-size $jit) (fxior PROT_READ PROT_WRITE)))
      (error 'jit-edit "mprotect failed")))

  (define (jit-exec $jit)
    (unless
      (zero? (mprotect (jit-address $jit) (jit-size $jit) (fxior PROT_READ PROT_EXEC)))
      (error 'jit-exec "mprotect failed")))

  (define (jit-destroy $jit)
    (unless
      (zero? (munmap (jit-address $jit) (jit-size $jit)))
      (error 'jit-destroy "munmap failed")))

  (define-scoped (with-jit $size)
    ($jit (%jit $size))
    (void)
    (jit-destroy $jit))
)
