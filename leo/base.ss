(library
  (leo base)
  (export)
  (import (scheme))
  (export
    (import
      (leo define)
      (leo define-syntax)
      (leo lambda)
      (leo let)
      (leo if)
      (leo in)
      (leo set)
      (leo then)
      (leo cond)
      (leo case)
      (leo assert)
      (leo then)
      (leo with)

      (only (scheme)
        when ; needed for case, cond

        boolean?
        not

        symbol?
        symbol=?
        symbol->string
        string->symbol

        char?
        char=?
        char<?
        char>?
        char<=?
        char>=?
        integer->char
        char->integer

        list?
        null?
        pair?

        car cdr
        caar cadr cdar cddr
        caaar caadr cadar cdaar caddr cdadr cddar cdddr
        caaaar caaadr caadar cadaar cdaaar cddaar cdadar cdaadr
        cadadr caaddr caddar cadddr cdaddr cddadr cdddar cddddr

        number?
        string?
        procedure?

        begin

        quote
        quasiquote
        unquote
        unquote-splicing

        and
        or

        eq?
        eqv?
        equal?

        and
        or

        complex?
        real-part
        imag-part
        make-rectangular
        make-polar
        magnitude
        angle

        sqrt
        exp
        expt
        log
        sin
        cos
        tan
        asin
        acos
        atan

        real?
        rational?
        numerator
        denominator
        rationalize

        exact?
        inexact?
        exact
        inexact

        integer?
        odd?
        even?
        gcd
        lcm
        exact-integer-sqrt

        = < > <= >=

        zero?
        positive?
        negative?

        for-each
        list
        length
        list-ref
        list-tail
        append
        reverse

        number->string
        string->number

        string
        make-string
        list->string
        string->list
        string-length
        string-ref
        string-copy
        substring

        string=?
        string<?
        string>?
        string<=?
        string>=?
        string-append
        string-for-each

        + - * /
        max
        min
        abs
        truncate
        floor
        ceiling
        round

        div
        mod
        div-and-mod

        div0
        mod0
        div0-and-mod0

        real-valued?
        rational-valued?
        integer-valued?

        nan?
        infinite?
        finite?

        vector-map
        vector-for-each
        vector
        vector?
        make-vector
        make-vector
        list->vector
        vector->list
        vector-length
        vector-ref
        vector-set!
        vector-fill!

        call-with-current-continuation
        call/cc

        values
        call-with-values

        dynamic-wind
        apply))))

