import
  leo scheme
  math

write << without logging >>
  string-append
    "Hello, "
    "world!"

write << with logging >>
  string-append
    logging "Hello, "
    logging "world!"

write << with logging and labels >>
  string-append
    logging: first, "Hello, "
    logging: second, "world!"
