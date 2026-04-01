import
  except: cond, from scheme
  leo check
  leo cond
  void

check void?
  cond
    when
      equal?: 2, 3
      "OK"

check equal?
  cond
    when
      equal?: 2, 2
      "OK"
    else
      "not OK"
  "OK"

check equal?
  cond
    when
      equal?: 2, 3
      "OK"
    else
      "not OK"
  "not OK"
