import leo scheme

write 'without logging

write string-append
  "Hello, "
  "Leo!"

write 'with logging

write string-append
  logging "Hello, "
  logging "Leo!"

write 'with logging and labels

write string-append
  logging: first, "Hello, "
  logging: second, "Leo!"
