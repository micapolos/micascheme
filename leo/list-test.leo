import leo scheme

check equal?
  with list
  null

check equal?
  with closed-list
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

check list? with closed-list
check list? closed-list: 1, 2, 3

check not list? open-list: 1
check not list? open-list: 1, 2, 3
