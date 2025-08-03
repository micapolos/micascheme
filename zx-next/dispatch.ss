(library (zx-next dispatch)
  (export
    dispatch
    dispatch-jp
    dispatch-call
    tail-dispatch
    ret-dispatch)
  (import
    (prefix (zx-next core) %)
    (asm base))

  ; input: A = dispatch index
  (%define-op-syntax tail-dispatch
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ tail entry ...)
          (lets
            ($tmps (generate-temporaries #'(entry ...)))
            #`(%with-labels (table)
              ; HL = dispatch table address
              (%ld %hl table)

              ; HL = dispatch address pointer
              #,(cond
                ((<= (length #'(entry ...)) #x7f)
                  #`(%begin
                    (%rlca)
                    (%add %hl %a)))
                (else
                  #`(%begin
                    (%ld %d 0)
                    (%ld %e %a)
                    (%inc de)
                    (%add %hl %de))))

              ; HL = dispatch address
              (%ld %a (%hl))
              (%inc %hl)
              (%ld %h (%hl))
              (%ld %l %a)

              ; dispatch
              (%jp (%hl))

              table
              #,@(map-with ($tmp $tmps) #`(%dw #,$tmp))

              #,@(map-with
                ($tmp $tmps)
                ($entry #'(entry ...))
                #`(%begin #,$tmp #,$entry tail))))))))

  (%define-op (dispatch-jp addr ...)
    (tail-dispatch (%begin) (%jp addr) ...))

  (%define-op (dispatch-call addr ...)
    (tail-dispatch (%begin) (%call addr) ...))

  (%define-op (ret-dispatch entry ...)
    (tail-dispatch (%ret) entry ...))

  (%define-op (dispatch entry ...)
    (%with-labels (end)
      (tail-dispatch (%jp end) entry ...)
      end))
)
