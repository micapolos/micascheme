import
  leo scheme
  leo record-type

let sequential
  point-record-type make-record-type
    "point"
    list
      'x
      'immutable y
  point-getter record-type-getter point-record-type
  point-setter! record-type-setter! point-record-type
  point with: record-constructor point-record-type, 10, 20
  in
    check equal?
      record-field-index: point-record-type, syntax x
      0
    check equal?
      record-field-index: point-record-type, syntax y
      1
    check raises
      record-field-index: point-record-type, syntax z

    check equal?
      with: point-getter syntax x, point
      10
    check equal?
      with: point-getter syntax y, point
      20
    check raises
      point-getter syntax z

    check procedure? point-setter! syntax x
    check raises point-setter! syntax y
    check raises point-setter! syntax z

    with: point-setter! syntax x, point, 30
    check equal?
      with: point-getter syntax x, point
      30

