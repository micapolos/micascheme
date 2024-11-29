(library (micalog std)
  (export inc dec set+ set- set-not when)
  (import (micalog) (only (micascheme) ...))

  (micalog
    (macro (inc name) (set name (wrap+ name 1)))
    (macro (dec name) (set name (wrap- name 1)))
    (macro (set+ name expr) (set name (wrap+ name expr)))
    (macro (set- name expr) (set name (wrap- name expr)))
    (macro (set-not name) (set name (not name)))
    (macro (when cond? body (... ...)) (cond (cond? body (... ...)))))
)
