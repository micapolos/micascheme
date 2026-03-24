import leo scheme

write quote without logging
write string-append
  "Hello, "
  "world!"

write quote with logging
write string-append
  logging "Hello, "
  logging "world!"

write quote with logging and labels
write string-append
  logging: first, "Hello, "
  logging: second, "world!"
