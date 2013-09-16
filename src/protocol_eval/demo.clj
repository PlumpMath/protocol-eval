;; Trying it all out
;; =================

(use 'protocol-eval.core :reload)

(evaluate '123 {})
(evaluate 'a (make-env nil {'a 10}))
(evaluate '(if true 10 20) {})
(evaluate '(if false 10 20) {})

(def minienv (make-env nil {'add #(apply + %)}))

(evaluate '(add 5 8) minienv)
(evaluate '((λ (x y) (add x x y)) 40 1) minienv)

(evaluate '(set! a 10) (make-env nil {}))

(let [e (make-empty-env)]
  (evaluate '(progn (set! a 20) (set! b 30) (set! a 10)) e)
  @e)

(let [e1 (make-evaluator)]
  (e1 '(set! a 200))
  (e1 '(set! b 30))
  (e1 '(set! c (λ (x y) (* x y))))
  (e1 '(c a b)))

(let [e (make-evaluator)]
  (e '(set! x 100))
  (e '(set! f (λ () (set! x 99))))
  (e '(f))
  (e 'x)
  )

(let [e (make-evaluator)]
  (e '(set! x 100))
  (e '(set! f (λ (x) (set! x 99))))
  (e '(f 10))
  (e 'x)
  )

(let [e (make-evaluator)]
  (e '(set! n "erik"))
  (e 'n)
  )

;; (defmacro run [& program]
;;   `((make-evaluator) '(progn ~@program)))
