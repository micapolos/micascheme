(library (sjasm)
  (export asm cspect)
  (import
    (micascheme)
    (sjasm emit)
    (sjasm code)
    (sjasm keywords)
    (code))
  (export
    (import
      (micascheme)
      (sjasm keywords)
      (sjasm emit)))

  (define-syntax (asm $syntax)
    (syntax-case $syntax ()
      ((_ line ...)
        #`(begin
          (emit #,(code-string (lines-code #'(line ...))))
          (emit "\n")))))

  (define-rule-syntax (cspect label stack)
    (lets
      (run
        (asm
          (savenex open "/tmp/main.nex" label stack)
          (savenex auto)
          (savenex close)
          (cspectmap "/tmp/main.map"))
        (display (emitted))
        (displayln "======================================================================")
        (save "/tmp/main.asm"))
      ($sjasm-result (system "sjasmplus --zxnext=cspect --lst=/tmp/main.lst /tmp/main.asm"))
      (run
        (when (= $sjasm-result 0)
          (system "cspect -map=/tmp/main.map /tmp/main.nex")))))
)
