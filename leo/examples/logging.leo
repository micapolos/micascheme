import leo scheme

print 'without logging

print string-append
  "Hello, "
  "Leo!"

print 'with logging

print string-append
  logging "Hello, "
  logging "Leo!"

print 'with logging and labels

print string-append
  logging first "Hello, "
  logging second "Leo!"
