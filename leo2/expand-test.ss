(import
  (leo2 expand)
  (prefix (leo2 comptime) %))

(check-expanded=?
  a-type
  (expanded (%type 1) a-type))

(check-expanded=?
  a-boolean
  (expanded (%type 0) (native 'a-boolean)))
