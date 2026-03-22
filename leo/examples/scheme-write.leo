import leo scheme

define-macro
  when
    scheme-pretty-print id: xs, ...
    displayln format
      "~s"
      syntax->datum apply
        top-level-syntax quote id
        list syntax id: xs, ...

scheme-pretty-print define x 10

scheme-pretty-print define
  comma-separated: x, y
  string-append: x, ", ", y
