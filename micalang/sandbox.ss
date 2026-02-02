(import (only (micascheme) logging) (scheme) (fib))

(optimize-level 3)

;; --- 1. Records for the Type Checker (NbE) ---
(define-record-type v-pi   (fields arg-type body))
(define-record-type v-neut (fields head args))

;; --- 2. Symbolic Evaluator (for Type Checking) ---
(define (do-apply f a)
  (cond [(procedure? f) (f a)]
        [(v-pi? f)      ((v-pi-body f) a)]
        [(v-neut? f)    (make-v-neut (v-neut-head f) (append (v-neut-args f) (list a)))]
        [else (error 'do-apply "Not a function" f)]))

(define (eval-term expr env)
  (cond
    [(or (number? expr) (boolean? expr)) expr]
    [(memq expr '(Type Nat Bool)) expr]
    [(list? expr)
     (case (car expr)
       [(var) (list-ref env (cadr expr))]
       [(pi)  (make-v-pi (eval-term (cadr expr) env)
                         (lambda (x) (eval-term (caddr expr) (cons x env))))]
       [(lam) (lambda (x) (eval-term (caddr expr) (cons x env)))]
       [(app) (do-apply (eval-term (cadr expr) env) (eval-term (caddr expr) env))]
       [(fix) (let* ([fix-body (eval-term (caddr expr) env)])
                (letrec ([rec (lambda (x) (do-apply (do-apply fix-body rec) x))]) rec))]
       [(<) (let ([v1 (eval-term (cadr expr) env)] [v2 (eval-term (caddr expr) env)])
               (if (and (number? v1) (number? v2)) (< v1 v2)
                   (make-v-neut 0 (list '< v1 v2))))]
       [(if) (let ([cond-v (eval-term (cadr expr) env)])
               (cond [(boolean? cond-v) (if cond-v (eval-term (caddr expr) env) (eval-term (cadddr expr) env))]
                     [else (make-v-neut 0 (list 'if cond-v))]))]
       [(sub)
          (let ([v1 (eval-term (cadr expr) env)]
             [v2 (eval-term (caddr expr) env)])
         (if (and (number? v1) (number? v2))
             (- v1 v2)
             (make-v-neut 0 (list 'sub v1 v2))))]
       [(add)  (let ([v1 (eval-term (cadr expr) env)] [v2 (eval-term (caddr expr) env)])
                 (if (and (number? v1) (number? v2)) (+ v1 v2) (make-v-neut 0 (list 'add v1 v2))))]
       [else expr])]
    [else expr]))

;; --- 3. The Quoter & Type Checker ---
(define (quote-term depth val)
  (cond
    [(memq val '(Type Nat Bool)) val]
    [(or (number? val) (boolean? val)) val]
    [(v-pi? val) `(pi ,(quote-term depth (v-pi-arg-type val))
                      ,(quote-term (+ depth 1) ((v-pi-body val) (make-v-neut depth '()))))]
    [(procedure? val) `(lam unknown ,(quote-term (+ depth 1) (val (make-v-neut depth '()))))]
    [(v-neut? val) (let ([index (- (- depth (v-neut-head val)) 1)])
                     (fold-left (lambda (acc arg) `(app ,acc ,(quote-term depth arg)))
                                `(var ,(max 0 index)) (v-neut-args val)))]
    [else val]))

(define (infer context env expr)
  (cond
    [(eq? expr 'Nat) 'Type]
    [(number? expr) 'Nat]
    [(eq? expr 'Bool) 'Type]
    [(boolean? expr) 'Bool]
    [(eq? expr 'Type) 'Type]
    [(list? expr)
     (case (car expr)
       [(var) (list-ref context (cadr expr))]
       [(pi)  (check context env (cadr expr) 'Type)
              (let ([arg-v (eval-term (cadr expr) env)])
                (check (cons arg-v context) (cons (make-v-neut (length context) '()) env) (caddr expr) 'Type) 'Type)]
       [(lam) (let ([arg-t (eval-term (cadr expr) env)])
                (check context env (cadr expr) 'Type)
                (make-v-pi arg-t (lambda (x) (infer (cons arg-t context) (cons x env) (caddr expr)))))]
       [(app) (let ([f-t (infer context env (cadr expr))])
                (if (v-pi? f-t) (begin (check context env (caddr expr) (v-pi-arg-type f-t)) (do-apply f-t (eval-term (caddr expr) env)))
                    (error 'infer "Expected Pi type" f-t)))]
       [(fix) (let ([t (eval-term (cadr expr) env)])
                (check context env (cadr expr) 'Type)
                (check context env (caddr expr) (make-v-pi t (lambda (_) t))) t)]
       [(<) (check context env (cadr expr) 'Nat) (check context env (caddr expr) 'Nat) 'Bool]
       [(if) (check context env (cadr expr) 'Bool)
             (let ([t (infer context env (caddr expr))]) (check context env (cadddr expr) t) t)]
       [(sub) (check context env (cadr expr) 'Nat) (check context env (caddr expr) 'Nat) 'Nat]
       [(add)  (check context env (cadr expr) 'Nat) (check context env (caddr expr) 'Nat) 'Nat]
       [else (error 'infer "Unknown" expr)])]
    [else (error 'infer "Syntax" expr)]))

(define (check context env expr expected-v)
  (let* ([actual-v (infer context env expr)] [d (length context)]
         [s1 (quote-term d actual-v)] [s2 (quote-term d expected-v)])
    (unless (equal? s1 s2) (error 'check "Mismatch" (list 'got s1 'expected s2)))))

;; --- 4. The Native Compiler (Stripping Interpreter Overhead) ---
(define (to-native expr depth)
  (cond
    [(or (number? expr) (boolean? expr)) expr]
    [(list? expr)
     (case (car expr)
       ;; Calculate the name based on depth: v(depth - index - 1)
       [(var) (string->symbol (format "v~a" (- (- depth (cadr expr)) 1)))]

       [(lam) (let ([v-name (string->symbol (format "v~a" depth))])
                `(lambda (,v-name) ,(to-native (caddr expr) (+ depth 1))))]

       [(app) `(,(to-native (cadr expr) depth) ,(to-native (caddr expr) depth))]

       [(fix)
        (let* ([fix-body (caddr expr)]
               [inner-lam (caddr fix-body)]
               [v-self (string->symbol (format "v~a" depth))]
               [v-arg  (string->symbol (format "v~a" (+ depth 1)))] )
          `(letrec ([,v-self (lambda (,v-arg)
                              ,(to-native (caddr inner-lam) (+ depth 2)))])
             ,v-self))]

       [(<)  `(< ,(to-native (cadr expr) depth) ,(to-native (caddr expr) depth))]
       [(if)  `(if ,(to-native (cadr expr) depth)
                   ,(to-native (caddr expr) depth)
                   ,(to-native (cadddr expr) depth))]
       [(sub) `(- ,(to-native (cadr expr) depth) ,(to-native (caddr expr) depth))]
       [(add) `(+ ,(to-native (cadr expr) depth) ,(to-native (caddr expr) depth))]
       [else (error 'to-native "Unknown" expr)])]
    [else expr]))

(define (compile-and-run-native expr type-expr)
  ;; Step 1: Verify logic via Type Checker
  (check '() '() expr (eval-term type-expr '()))
  ;; Step 2: Compile to machine code via Chez eval
  (eval (logging (to-native expr 0)) (scheme-environment)))

;; --- 5. Examples ---


;; Standard setup as before...
(define fib-body
  '(lam (pi Nat Nat)
     (lam Nat
       (if (< (var 0) 2)
           (var 0)
           (add (app (var 1) (sub (var 0) 1))
                (app (var 1) (sub (var 0) 2)))))))

(define fib-program `(fix (pi Nat Nat) ,fib-body))

;; Compile
(define fib-native (compile-and-run-native fib-program '(pi Nat Nat)))

;; THE CORRECT CALL
(newline)
(display "--- Benchmarking Recursive Fibonacci ---")
(newline)
;; On an M4, Fib(40) should take roughly 0.5 to 1.5 seconds natively.
(time (let ([res (fib-native 40)])
        (format #t "Fib(40) = ~a\n" res)))

(time (let ([res (fib 40)])
        (format #t "Fib(40) = ~a\n" res)))
