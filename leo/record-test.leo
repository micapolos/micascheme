import
  except: define, let, from scheme
  leo define
  leo check
  leo record
  leo let
  leo in
  leo make
  leo is?
  leo get
  boolean

define record point: x, y

let
  test-point make point: 10, 20
  in
    check equal?: is? record test-point, true
    check equal?: is? point test-point, true
    check equal?: get point x test-point, 10
    check equal?: get point y test-point, 20
