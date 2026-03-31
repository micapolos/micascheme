import
  leo scheme
  leo current

check current? current 10
check current? current: 10, 20, 30

check equal?: current-ref current 10, 10

check equal?
  current-ref current: 10, 20, 30
  30
