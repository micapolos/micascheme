import
  except: let, from scheme
  leo check
  leo record
  leo let
  leo in
  leo make
  leo is?
  leo get
  boolean

leo-define-record point: x, y

let
  test-point make point: 10, 20
  in
    check equal?: is? record test-point, true
    check equal?: is? point test-point, true
    check equal?: get point x test-point, 10
    check equal?: get point y test-point, 20
