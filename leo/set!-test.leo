import
  except: define, set!, let, from scheme
  check
  leo define
  leo let
  leo in
  leo ref
  leo set!

let
  x 10
  in
    check equal?: x, 10
    set! x 20
    check equal?: x, 20

let
  bv bytevector: 10, 20, 30
  in
    check equal?
      ref bytevector u8: bv, 1
      20
    set! bytevector u8: bv, 1, 120
    check equal?
      ref bytevector u8: bv, 1
      120
