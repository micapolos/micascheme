(library (zx-next dispatch)
  (export
    tail-dispatch
    ret-dispatch
    jp-dispatch
    dispatch
    dispatch-jp
    dispatch-call)
  (import
    (prefix (zx-next core) %)
    (zx-next lookup)
    (asm base))

  (%define-op-syntax tail-dispatch
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ tail entry ...)
          (lets
            ($tmps (generate-temporaries #'(entry ...)))
            #`(%with-labels (table)
              (%ld %hl table)

              (%call
                #,(if (<= (length #'(entry ...)) #x7f)
                  #'lookup-word-7
                  #'lookup-word))

              (%ex %de %hl)

              (%jp (%hl))

              table
              #,@(map-with ($tmp $tmps) #`(%dw #,$tmp))

              #,@(map-with
                ($tmp $tmps)
                ($entry #'(entry ...))
                #`(%begin #,$tmp #,$entry tail))))))))

  (%define-op (ret-dispatch entry ...)
    (tail-dispatch (%ret) entry ...))

  (%define-op (jp-dispatch address entry ...)
    (tail-dispatch (%jp address) entry ...))

  (%define-op (dispatch-jp addr ...)
    (tail-dispatch (%begin) (%jp addr) ...))

  (%define-op (dispatch-call addr ...)
    (tail-dispatch (%begin) (%call addr) ...))

  (%define-op (dispatch entry ...)
    (%with-labels (end)
      (tail-dispatch (%jp end) entry ...)
      end))
)
