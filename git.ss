(library (git)
  (export git add all changed? commit push)
  (import
    (only (micascheme)
      define-rules-syntaxes
      define-keywords
      keywords
      system
      zero?
      format))

  (define-keywords add all changed? commit push)

  (define-rules-syntaxes (keywords add all changed? commit push)
    ((git add all)
      (system "git add -A"))
    ((git changed?)
      (zero? (system "git status --porcelain=v1 | grep -q .")))
    ((git commit $message)
      (system (format "git commit -m ~s" $message)))
    ((git push)
      (system "git push")))
)
