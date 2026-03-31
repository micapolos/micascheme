import leo scheme

check equal?
  list:
  null

check equal?
  closed-list:
  null

check equal?
  closed-list: 1, 2, 3
  list: 1, 2, 3

check equal?
  open-list: 1
  1

check equal?
  open-list: 1, 2, 3
  list: 1, 2, and 3
