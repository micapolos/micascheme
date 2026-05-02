import
  leo scheme
  leo version

define leo string-append: "Leo ", version

define author "Michal Pociecha-Los"

define
  greeting recipient
  'greeting
    using' leo
    from' author
    to' recipient

print greeting "Alex Johnson"
print greeting "John Appleseed"
print greeting "Chris Parker"
