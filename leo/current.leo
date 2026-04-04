library
  leo current
  export
    rename %current current
    current?
    current-ref
    current-map
    current-lets
    current-lets?
    current-switch
    list->current
    replace-current
    current-append
    apply-current
    current-or
    current-and
  import
    leo scheme
    data
    monadic

  data current procedure

  define-syntax
    when
      %current: x, xs, ...
      current lambda: x, xs, ...
    when
      current-let1: val expr, body
      %current let
        val current-ref expr
        in current-ref body
    when
      current-ref $current
      with current-procedure $current

  define-monadic current
