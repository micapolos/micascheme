import
  only: equal?, syntax, quote, immutable, mutable, from scheme
  leo check
  leo field-spec

check equal?
  field-spec-normalize syntax x
  'immutable: ptr, x

check equal?
  field-spec-normalize syntax immutable x
  'immutable: ptr, x

check equal?
  field-spec-normalize syntax mutable x
  'mutable: ptr, x

check equal?
  field-spec-normalize syntax integer-32 x
  'immutable: integer-32, x

check equal?
  field-spec-normalize syntax immutable integer-32 x
  'immutable: integer-32, x

check equal?
  field-spec-normalize syntax mutable integer-32 x
  'mutable: integer-32, x
