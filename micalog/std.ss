(library (micalog std)
  (export inc dec set+ set- set-not)
  (import (micalog))

  (micalog
    (macro (inc name) (set-take name (+ name 1)))
    (macro (dec name) (set-take name (- name 1)))
    (macro (set+ name expr) (set-take name (+ name expr)))
    (macro (set- name expr) (set-take name (- name expr)))
    (macro (set-not name) (set name (not name))))
)
