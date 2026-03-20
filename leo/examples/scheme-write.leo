import leo scheme

define-macro
  when
    scheme-write id: xs, ...
    displayln format
      "~s"
      syntax->datum apply
        top-level-syntax quote id
        list syntax id: xs, ...

scheme-write define x 10

scheme-write define
  comma-separated: x, y
  string-append: x, ", ", y

scheme-write if: 1, then 2, else 3

