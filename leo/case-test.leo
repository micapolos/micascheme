import
  only: equal?, values, when, else, from scheme
  leo check
  leo case

check equal?
  case
    1
    when: 1, "one"
    when: 2, "two"
  "one"

check equal?
  case
    2
    when: 1, "one"
    when: 2, "two"
  "two"

check equal?
  case
    2
    when: 1, "one"
    else "two"
  "two"

check equal?
  case
    3
    when
      values: 1, 2, 3, 4, 5
      "12345"
    when
      values: 6, 7, 8, 9, 0
      "67890"
  "12345"

check equal?
  case
    8
    when
      values: 1, 2, 3, 4, 5
      "12345"
    when
      values: 6, 7, 8, 9, 0
      "67890"
  "67890"

check equal?
  case
    12
    when
      values: 1, 2, 3, 4, 5
      "12345"
    when
      values: 6, 7, 8, 9, 0
      "67890"
    else
      "other"
  "other"

