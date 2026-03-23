import leo scheme

pretty-print quote without logging
pretty-print string-append
  "Hello, "
  "world!"

pretty-print quote with logging
pretty-print string-append
  logging "Hello, "
  logging "world!"

pretty-print quote with logging and labels
pretty-print string-append
  logging: first, "Hello, "
  logging: second, "world!"
