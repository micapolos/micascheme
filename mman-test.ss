(import (scheme) (mman) (lets) (check) (arm64))

(define page
  (mmap
    0 ; address, null
    #x10000 ; page size
    (fxior PROT_READ PROT_WRITE)
    (fxior MAP_PRIVATE MAP_ANONYMOUS)
    -1 ; fd, must be -1
    0 ; offset, must be 0
    ))

(check (not (= page MAP_FAILED)))

; ARM64 machine code
(foreign-set! 'unsigned-32 page 0 (add x0 x0 x1))
(foreign-set! 'unsigned-32 page 4 (ret))

(check (fxzero? (mprotect page #x10000 (fxior PROT_READ PROT_EXEC))))

(define arm64-add (foreign-procedure page (int int) int))

(when
  (equal? (machine-type) 'tarm64osx)
  (check (= (arm64-add 10 20) 30)))

(check (fxzero? (munmap page #x10000)))
