(import (scheme) (mman) (lets) (check))

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

(check (fxzero? (mprotect page #x10000 (fxior PROT_READ PROT_EXEC))))

(mflush page #x10000)

(check (fxzero? (munmap page #x10000)))
