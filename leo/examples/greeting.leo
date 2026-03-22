import
  leo scheme
  leo version

define leo string-append: "Leo ", version
define author "Michal Pociecha-Los"

define
  greeting recipient
  << greeting
    using >> leo
    from >> author
    to >> recipient

pretty-print greeting "Alex Johnson"
pretty-print greeting "John Appleseed"
pretty-print greeting "Chris Parker"
