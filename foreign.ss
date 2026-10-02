(library (foreign)
  (export
    foreign-alloc-0
    foreign-free-0

    with-foreign-alloc
    with-foreign-alloc-0

    with-locked-object
    with-object->reference-address
    with-vector-ftype-pointer-and-count

    ftype-alloc
    with-ftype-alloc

    foreign

    foreign-string-length
    foreign-string

    foreign-int
    foreign-uptr
    foreign-u8
    foreign-u32

    foreign-set-int!
    foreign-set-uptr!
    foreign-set-u8!
    foreign-set-u32!)
  (import (scheme) (syntax) (syntaxes) (dynamic-wind) (lets) (scoped) (procedure) (port))

  (define (foreign-alloc-0 size)
    (if (zero? size) 0 (foreign-alloc size)))

  (define (foreign-free-0 ptr)
    (if (zero? ptr) (void) (foreign-free ptr)))

  (define-scoped (foreign size)
    ($foreign (foreign-alloc size))
    (foreign-free $foreign))

  (define-rule-syntax (with-foreign-alloc (id size) body ...)
    (with-dynamic-wind
      (id (foreign-alloc size))
      body ...
      (foreign-free id)))

  (define-rule-syntax (with-foreign-alloc-0 (id size) body ...)
    (with-dynamic-wind
      (id (foreign-alloc-0 size))
      body ...
      (foreign-free-0 id)))

  (define-rule-syntax (with-locked-object (id obj) body ...)
    (with-dynamic-wind
      (id
        (lets
          (var obj)
          (lock-object var)
          var))
      body ...
      (unlock-object id)))

  (define-rule-syntax (with-object->reference-address (id obj) body ...)
    (with-locked-object (locked-obj obj)
      (let ((id (object->reference-address locked-obj))) body ...)))

  (define-rule-syntax (with-vector-ftype-pointer-and-count (id length ftype vector) body)
    (lets
      (vector-var vector)
      (length (vector-length vector-var))
      (with-foreign-alloc-0 (ptr (* (ftype-sizeof ftype) length))
        (let ((id (make-ftype-pointer ftype ptr)))
          (repeat-indexed (index length)
            (ftype-set! ftype () id index (vector-ref vector-var index)))
          body))))

  (define-rules-syntax
    ((ftype-alloc ftype)
      (ftype-alloc ftype 1))
    ((ftype-alloc ftype size)
      (make-ftype-pointer ftype
        (foreign-alloc (fx* (ftype-sizeof ftype) size)))))

  (define-rules-syntax
    ((with-ftype-alloc (id ftype) body ...)
      (with-ftype-alloc (id ftype 1) body ...))
    ((with-ftype-alloc (id ftype size) body ...)
      (with-foreign-alloc (ptr (* (ftype-sizeof ftype) size))
        (lets (id (make-ftype-pointer ftype ptr))
          body ...))))

  (define (foreign-string-length address)
    (let loop ((offset 0))
      (lets
        (u8 (foreign-ref 'unsigned-8 address offset))
        (if (zero? u8)
          offset
          (loop (add1 offset))))))

  (define (foreign-string $address)
    (utf8->string
      (with-bytevector-output-port $port
        (let $loop (($offset 0))
          (lets
            ($u8 (foreign-ref 'unsigned-8 $address $offset))
            (cond
              ((zero? $u8) (void))
              (else
                (put-u8 $port $u8)
                ($loop (add1 $offset)))))))))

  (define-rules-syntax
    ((foreign-int $address)
      (foreign-int $address 0))
    ((foreign-int $address $offset)
      (foreign-ref 'int $address $offset)))

  (define-rules-syntax
    ((foreign-uptr $address)
      (foreign-uptr $address 0))
    ((foreign-uptr $address $offset)
      (foreign-ref 'uptr $address $offset)))

  (define-rules-syntax
    ((foreign-u8 $address)
      (foreign-u8 $address 0))
    ((foreign-u8 $address $offset)
      (foreign-ref 'unsigned-8 $address $offset)))

  (define-rules-syntax
    ((foreign-u32 $address)
      (foreign-u32 $address 0))
    ((foreign-u32 $address $offset)
      (foreign-ref 'unsigned-32 $address $offset)))

  (define-rules-syntax
    ((foreign-set-int! $address $int)
      (foreign-set-int! $address 0 $int))
    ((foreign-set-int! $address $offset $int)
      (foreign-set! 'int $address $offset $int)))

  (define-rules-syntax
    ((foreign-set-uptr! $address $uptr)
      (foreign-set-uptr! $address 0 $uptr))
    ((foreign-set-uptr! $address $offset $uptr)
      (foreign-set! 'uptr $address $offset $uptr)))

  (define-rules-syntax
    ((foreign-set-u8! $address $u8)
      (foreign-set-u8! $address 0 $u8))
    ((foreign-set-u8! $address $offset $u8)
      (foreign-set! 'unsigned-8 $address $offset $u8)))

  (define-rules-syntax
    ((foreign-set-u32! $address $u32)
      (foreign-set-u32! $address 0 $u32))
    ((foreign-set-u32! $address $offset $u32)
      (foreign-set! 'unsigned-32 $address $offset $u32)))
)
