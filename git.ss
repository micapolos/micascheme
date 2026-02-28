(library (git)
  (export
    git-add
    git-clean?
    git-commit
    git-push)
  (import (micascheme))

  (define-rules-syntaxes
    ((git-add)
      (system "git add -A"))
    ((git-clean?)
      (zero? (system "! git status --porcelain=v1 | grep -q .")))
    ((git-commit message)
      (system (format "git commit -m ~s" message)))
    ((git-push)
      (system "git push")))
)
