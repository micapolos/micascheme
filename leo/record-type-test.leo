import
  leo scheme
  leo record-type

let sequential
  point-record-type make-record-type
    "point"
    list: 'x, 'y
  point-getter record-type-getter point-record-type
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
