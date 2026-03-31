library
  leo current
  export
    rename %current current
    current?
    current-ref
    current-bind
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

  define
    syntax
      when
        %current body
        current lambda body

    lambda
      current-ref $current
      with current-procedure $current

    lambda
      current-bind: $current, $binder
      %current let
        $ref current-ref $current
        in current-ref with: $binder, $ref

  define-monadic current
