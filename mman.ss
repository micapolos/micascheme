(library (mman)
  (export
    PROT_NONE
    PROT_READ
    PROT_WRITE
    PROT_EXEC

    MAP_PRIVATE
    MAP_ANONYMOUS
    MAP_FAILED

    mmap
    munmap
    mprotect
    mflush)
  (import
    (chezscheme)
    (shared-library))

  (define *mman* (load-shared-library "c"))

  (define *system*
    (when
      (equal? (machine-type) 'tarm64osx)
      (load-shared-object "libSystem.B.dylib")))

  (define PROT_NONE #x0)
  (define PROT_READ #x1)
  (define PROT_WRITE #x2)
  (define PROT_EXEC #x4)

  (define MAP_PRIVATE #x0002)
  (define MAP_ANONYMOUS #x1000)
  (define MAP_FAILED -1)

  (define mmap (foreign-procedure "mmap" (iptr size_t int int int int) iptr))
  (define munmap (foreign-procedure "munmap" (iptr size_t) int))
  (define mflush
    (if (equal? (machine-type) 'tarm64osx)
      (foreign-procedure "sys_icache_invalidate" (iptr size_t) void)
      (lambda (addr size) (void))))
  (define mprotect (foreign-procedure "mprotect" (iptr size_t int) int))
)
