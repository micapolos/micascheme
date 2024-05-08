(library (foreign)
  (export
    foreign-alloc-0
    foreign-free-0

    with-foreign-alloc
    with-foreign-alloc-0

    with-locked-object
    with-object->reference-address
    with-vector-ftype-pointer-and-count
    with-ftype-alloc

    foreign-string-length
    foreign-string)
  (import (scheme) (syntax) (syntaxes) (dynamic-wind) (lets) (procedure))

  (define (foreign-alloc-0 size)
    (if (zero? size) 0 (foreign-alloc size)))

  (define (foreign-free-0 ptr)
    (if (zero? ptr) (void) (foreign-free ptr)))

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
      (let ((id (object->reference-address locked-obj)) body ...))))

  (define-rule-syntax (with-vector-ftype-pointer-and-count (id length ftype vector) body)
    (lets
      (vector-var vector)
      (length (vector-length vector-var))
      (with-foreign-alloc-0 (ptr (* (ftype-sizeof ftype) length))
        (let ((id (make-ftype-pointer ftype ptr)))
          (repeat-indexed length index
            (ftype-set! ftype () id index (vector-ref vector-var index)))
          body))))

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
    (lets
      ((values $port $close) (open-bytevector-output-port))
      ($bytevector
        (let $loop (($offset 0))
          (lets
            ($u8 (foreign-ref 'unsigned-8 $address $offset))
            (cond
              ((zero? $u8) ($close))
              (else
                (put-u8 $port $u8)
                ($loop (add1 $offset)))))))
      (utf8->string $bytevector)))
)
