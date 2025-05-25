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
          (savenex open "/Users/micapolos/git/zexy/built/scheme-demo.nex" label stack)
          (savenex auto)
          (savenex close)
          (cspectmap "/Users/micapolos/git/zexy/built/scheme-demo.map"))
        (display (emitted))
        (displayln "========================================================================")
        (save "/Users/micapolos/git/zexy/nex/scheme-demo.asm"))
      ($sjasm-result
        (system "sjasmplus --zxnext=cspect --lst=/Users/micapolos/git/zexy/built/scheme-demo.lst /Users/micapolos/git/zexy/nex/scheme-demo.asm"))
      (run
        (when (= $sjasm-result 0)
          (system "cspect -map=/Users/micapolos/git/zexy/built/scheme-demo.map /Users/micapolos/git/zexy/built/scheme-demo.nex")))))
)
