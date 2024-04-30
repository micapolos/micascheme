(library (tim type)
  (export
    index-type? index-type index-type-size
    array-type? array-type array-type-element array-type-size
    string-type? string-type
    universe? universe universe-depth
    type->syntax)
  (import (micascheme))

  (data (index-type size))
  (data (array-type element size))
  (data (string-type))
  (data (universe depth))

  (define (type->syntax $type)
    (switch-exclusive $type
      ((universe? $universe)
        #`(universe #'#,(universe-depth $universe)))
      ((index-type? (index-type $size))
        #`(index-type #'#,$size))
      ((array-type? (array-type $element $size))
        #`(array-type #,(type->syntax $element) #'#,$size))
      ((string-type? _)
        #`(string-type))))
)
