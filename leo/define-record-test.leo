import
  only
    equal?, not, procedure?
    record?, record-type-descriptor?, record-type-descriptor
    immutable, mutable
    from scheme
  leo check
  leo with
  leo let
  leo define-record

define-record point with: x, y

define-record color with
  mutable unsigned-8 red
  mutable unsigned-8 green
  mutable unsigned-8 blue

check procedure? make-point
check procedure? point?
check procedure? point-x
check procedure? point-y

check procedure? make-color
check procedure? color?
check procedure? color-red
check procedure? color-green
check procedure? color-blue

let
  with
    test-point make-point: 10, 20
    test-color make-color: 10, 20, 30
  check record-type-descriptor? record-type-descriptor point
  check record? test-point
  check point? test-point
  check not point? test-color
  check equal?: point-x test-point, 10
  check equal?: point-y test-point, 20

  check record-type-descriptor? record-type-descriptor color
  check record? test-color
  check color? test-color
  check not color? test-point
  check equal?: color-red test-color, 10
  check equal?: color-green test-color, 20
  check equal?: color-blue test-color, 30

  set-color-red!: test-color, 110
  set-color-green!: test-color, 120
  set-color-blue!: test-color, 130
  check equal?: color-red test-color, 110
  check equal?: color-green test-color, 120
  check equal?: color-blue test-color, 130
