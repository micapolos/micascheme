(library (leo transform-ftype)
  (export
    transform-signedness
    transform-size
    transform-bits-field
    transform-field
    transform-ftype)
  (import
    (except (chezscheme) syntax-error)
    (syntax)
    (system)
    (pair)
    (switch)
    (keyword)
    (leo syntax-error))

  (define (transform-field field)
    (syntax-case field ()
      ((name ftype)
        (keyword? name)
        #`(name #,(transform-ftype #'ftype)))
      (_
        (syntax-error field '(expected (name ftype))))))

  (define (transform-signedness signedness)
    (syntax-case signedness ()
      (signed
        (free-keyword? signed)
        #'signed)
      (unsigned
        (free-keyword? unsigned)
        #'unsigned)
      (_
        (syntax-error signedness
          '(expected (one (of signed unsigned)))))))

  (define (transform-endianness endianness)
    (syntax-case endianness ()
      (native
        (free-keyword? native)
        #'native)
      (swapped
        (free-keyword? swapped)
        #'swapped)
      (big
        (free-keyword? big)
        #'big)
      (little
        (free-keyword? little)
        #'little)
      (_
        (syntax-error endianness
          '(expected (one (of native swapped big little)))))))

  (define (transform-size size)
    (syntax-case size ()
      (size
        (and (integer? (datum size)) (not (negative? (datum size))))
        #'size)
      (_
        (syntax-error size '(expected (non (negative integer)))))))

  (define (transform-bits-field bits-field)
    (syntax-case bits-field ()
      ((name (signedness size))
        #`(name
          #,(transform-signedness #'signedness)
          #,(transform-size #'size)))
      (_
        (syntax-error bits-field
          '(expected (name (signedness size)))))))

  (define (transform-ftype ftype)
    (syntax-case ftype ()
      (id
        (keyword? id)
        #'id)
      ((bits fields ...)
        (free-keyword? bits)
        #`(bits #,@(map transform-bits-field #'(fields ...))))
      ((struct fields ...)
        (free-keyword? struct)
        #`(struct #,@(map transform-field #'(fields ...))))
      ((union fields ...)
        (free-keyword? union)
        #`(union #,@(map transform-field #'(fields ...))))
      ((array size ftype)
        (free-keyword? array)
        #`(array
          #,(transform-size #'size)
          #,(transform-ftype #'ftype)))
      ((* ftype)
        (free-keyword? *)
        #`(* #,(transform-ftype #'ftype)))
      ((packed ftype)
        (free-keyword? packed)
        #`(packed #,(transform-ftype #'ftype)))
      ((unpacked ftype)
        (free-keyword? unpacked)
        #`(unpacked #,(transform-ftype #'ftype)))
      ((endian (endianness ftype))
        (free-keyword? endian)
        #`(endian
          #,(transform-endianness #'endianness)
          #,(transform-ftype #'ftype)))
      (_
        (syntax-error ftype
          '(expected (one (of
            ftype-name
            (bits fields ...)
            (struct fields ...)
            (union fields ...)
            (array size ftype)
            (* ftype)
            (packed *ftype)
            (unpacked ftype)
            (endian endianness ftype))))))))
)
