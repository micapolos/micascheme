import
  chezscheme
  boolean
  syntax
  leo check
  leo adjective

define-keyword my
define-property: my, adjective, true

check equal?: adjective? my, true
check equal?: adjective? his, false
