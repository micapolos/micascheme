import leo scheme

print 'quoted string-append: "foo", "bar"

print 'quasiquoted and unquoted' string-append: "foo", "bar"

print 'quasiquoted and unquoted-splicing'... list: "foo", "bar"

print 'circle
  radius' add: 10, 20
  center point
    x' sin 10
    y' cos 10
  labels'... list: "nice", "looking", "circle"

print 'quoted 'circle
  radius' add: 10, 20
  center point
    x' sin 10
    y' cos 10
  labels'... list: "nice", "looking", "circle"
