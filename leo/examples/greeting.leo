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

write greeting "Alex Johnson"
write greeting "John Appleseed"
write greeting "Chris Parker"
