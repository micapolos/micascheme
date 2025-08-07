(library (zx-next dispatch)
  (export
    tail-dispatch
    ret-dispatch
    dispatch
    dispatch-jp
    dispatch-call)
  (import
    (prefix (zx-next core) %)
    (zx-next lookup)
    (asm base))

  ; All registers preserved except HL.
  (%define-op-syntax tail-dispatch
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ tail entry ...)
          (lets
            ($tmps (generate-temporaries #'(entry ...)))
            #`(%with-labels (table)
              (%preserve (%de)
                (%ld %hl table)

                (%call
                  #,(if (zero? (fxand #x80 (length #'(entry ...))))
                    #'lookup-word-7
                    #'lookup-word))

                (%ex %de %hl))
              (%jp (%hl))

              table #,@(map-with ($tmp $tmps) #`(%dw #,$tmp))

              #,@(map-with
                ($tmp $tmps)
                ($entry #'(entry ...))
                #`(%begin #,$tmp #,$entry tail))))))))

  (%define-op (ret-dispatch entry ...)
    (tail-dispatch (%ret) entry ...))

  (%define-op (dispatch-jp addr ...)
    (tail-dispatch (%begin) (%jp addr) ...))

  (%define-op (dispatch-call addr ...)
    (dispatch (%call addr) ...))

  (%define-op (dispatch entry ...)
    (%with-labels (end)
      (tail-dispatch (%jp end) entry ...)
      end))
)
