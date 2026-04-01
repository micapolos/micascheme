import
  leo check
  leo math
  only: not, from scheme

check number=?
  add: 10, 20
  30

check number=?
  subtract: 30, 20
  10

check number=?
  multiply: 10, 20
  200

check number=?
  divide: 200, 10
  20

check less?: 10, 20
check not less?: 10, 10

check less/equal?: 10, 20
check less/equal?: 10, 10
check not less/equal?: 10, 9

check greater?: 20, 10
check not greater?: 10, 10

check greater/equal?: 20, 10
check greater/equal?: 10, 10
check not greater/equal?: 9, 10
