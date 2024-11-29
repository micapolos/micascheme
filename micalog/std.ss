(library (micalog std)
  (export
    zero?
    in-range< in-range<=
    inc dec
    set+ set- set-not
    when)
  (import (micalog) (only (micascheme) ...))

  (micalog
    (macro (zero? x) (= x 0))

    (macro (in-range< a min max) (and (>= a min) (< a max)))
    (macro (in-range<= a min max) (and (>= a min) (<= a max)))

    (macro (inc name) (set name (wrap+ name 1)))
    (macro (dec name) (set name (wrap- name 1)))
    (macro (set+ name expr) (set name (wrap+ name expr)))
    (macro (set- name expr) (set name (wrap- name expr)))
    (macro (set-not name) (set name (not name)))

    (macro (when cond? body (... ...)) (cond (cond? body (... ...)))))
)
