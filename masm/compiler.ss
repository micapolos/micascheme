(library (masm compiler)
  (export
    var? var var-name var-type

    copy? copy copy-src copy-dst copy-size

    type-size
    types-offset
    types-copy-instr)
  (import
    (except (micascheme) module)
    (masm model))

  (data (var name type))
  (data (body scope instrs))

  (data (copy src dst size))

  (define (type-size $type)
    (type-switch $type
      ((int? $int)
        (int-switch $int
          ((i8? _) 1)
          ((i16? _) 2)))
      ((arrow? _) 2)))

  (define (offset+types-offset $offset $types $idx)
    (cond
      ((zero? $idx) $offset)
      (else
        (lets
          ((pair $type $types) $types)
          (offset+types-offset
            (+ $offset (type-size $type))
            $types
            (sub1 $idx))))))

  (define (types-offset $types $idx)
    (offset+types-offset 0 $types $idx))

  (define (types-copy-instr $types $src-idx $dst-idx)
    (copy
      (types-offset $types $src-idx)
      (types-offset $types $dst-idx)
      (type-size (list-ref $types $src-idx))))
)
