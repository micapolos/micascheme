import leo scheme

write 'without logging

write string-append
  "Hello, "
  "world!"

write 'with logging

write string-append
  logging "Hello, "
  logging "world!"

write 'with logging and labels

write string-append
  logging: first, "Hello, "
  logging: second, "world!"
