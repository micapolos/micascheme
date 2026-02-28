(library (git)
  (export
    git add all
    git changed?
    git commit
    git push)
  (import (except (micascheme) push))

  (define-keywords add all changed? commit push)

  (define-rules-syntaxes (literals add all changed? commit push)
    ((git add all)
      (system "git add -A"))
    ((git changed?)
      (zero? (system "git status --porcelain=v1 | grep -q .")))
    ((git commit message)
      (system (format "git commit -m ~s" message)))
    ((git push)
      (system "git push")))
)
