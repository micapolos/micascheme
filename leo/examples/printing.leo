import leo scheme

print 'without printing

print string-append
  "Hello, "
  "Leo!"

print 'with printing

print string-append
  printing "Hello, "
  printing "Leo!"

print 'with printing and labels

print string-append
  printing: first, "Hello, "
  printing: second, "Leo!"
