import
  except: define, let, from scheme
  leo define
  leo check
  leo record
  leo let
  leo in
  leo adjective
  leo comment

define record point: x, y

define record color
  mutable unsigned-8 red
  mutable unsigned-8 green
  mutable unsigned-8 blue

check procedure? x-y->point
check procedure? point?
check procedure? point-x
check procedure? point-y

check procedure? red-green-blue->color
check procedure? color?
check procedure? color-red
check procedure? color-green
check procedure? color-blue

comment function syntax
let
  test-point x-y->point: 10, 20
  test-color red-green-blue->color: 10, 20, 30
  in
    check record-type-descriptor? point-type
    check record? test-point
    check point? test-point
    check not point? test-color
    check equal?: point-x test-point, 10
    check equal?: point-y test-point, 20

    check record-type-descriptor? color-type
    check record? test-color
    check color? test-color
    check not color? test-point
    check equal?: color-red test-color, 10
    check equal?: color-green test-color, 20
    check equal?: color-blue test-color, 30

    color-red-set!: test-color, 110
    color-green-set!: test-color, 120
    color-blue-set!: test-color, 130
    check equal?: color-red test-color, 110
    check equal?: color-green test-color, 120
    check equal?: color-blue test-color, 130

comment macro syntax
let
  test-point point: x 10, y 20
  test-color color: red 10, green 20, blue 30
  in
    check equal?: point x test-point, 10
    check equal?: point y test-point, 20

    check equal?: color red test-color, 10
    check equal?: color green test-color, 20
    check equal?: color blue test-color, 30

    color set! red: test-color, 110
    color set! green: test-color, 120
    color set! blue: test-color, 130

    check equal?: color red test-color, 110
    check equal?: color green test-color, 120
    check equal?: color blue test-color, 130
