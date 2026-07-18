(library (asm8 type)
  (export
    make-type
    type?
    type-id
    type-name
    type-alignment
    type-size

    make-primitive-type
    primitive-type?

    make-field
    field?
    field-name
    field-location

    make-struct-type
    struct-type?
    struct-type-fields

    make-array-type
    array-type?
    array-type-length
    array-type-element-type

    make-location
    location?
    location-offset
    location-type

    array-ref?
    struct-ref?
    fields-ref?
    field-ref?)
  (import (scheme))

  (define-record-type type
    (fields id name alignment size))

  (define-record-type primitive-type
    (parent type))

  (define-record-type field
    (fields name location))

  (define-record-type array-type
    (parent type)
    (fields length element-type))

  (define-record-type struct-type
    (parent type)
    (fields fields))

  (define-record-type location
    (fields offset type))

  (define (array-ref? $array $index)
    (and
      (>= $index 0)
      (< $index (array-type-length $array)
      (*
        (type-size (array-type-element-type $array))
        (array-type-length $array)))))

  (define (struct-ref? $struct $name)
    (fields-ref? (struct-type-fields $struct) $name 0))

  (define (fields-ref? $fields $name $start-index)
    (cond
      ((= $start-index (vector-length $fields)) #f)
      (else
        (or
          (field-ref? (vector-ref $fields $start-index) $name)
          (fields-ref? $fields $name (+ $start-index 1))))))

  (define (field-ref? $field $name)
    (and
      (symbol=? (field-name $field) $name)
      (field-location $field)))
)
