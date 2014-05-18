;; 使用全局变量
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))
;; 使用局部变量
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))
;; 创建不同初始balance的withdraw函数
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))

;; 比较完整的帐号操作
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch op)
    (cond ((eq? op 'withdraw)
	   withdraw)
	  ((eq? op 'deposit)
	   deposit)
	  (else (error "MAKE-ACCOUNT" "Unknown request" m))))
  dispatch)

;; Exercise 3.1
(define (make-accumulator sum)
  (lambda (val)
    (set! sum (+ val sum))
    sum))

;; Exercise 3.2
(define (make-monitored procedure)
  (let ((counter 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?)
	     counter)
	    ((eq? m 'reset-count)
	     (set! counter 0))
	    (else
	     (set! counter (+ counter 1))
	     (procedure m))))
    dispatch))

;; Exercise 3.3
(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch user-passwd op)
    (if (eq? passwd user-passwd)
	(cond ((eq? op 'withdraw)
	       withdraw)
	      ((eq? op 'deposit)
	       deposit)
	      (else (error "MAKE-ACCOUNT" "Unknow request" op)))
	 (error "MAKE-ACCOUNT" "Incorrect password" user-passwd)))
  dispatch)

;; Exercise 3.4
(define (make-account balance passwd)
  (let ((incorrect-counter 0))
    (define (reset-counter)
      (set! incorrect-counter 0))
    (define (inc-counter)
      (set! incorrect-counter (+ incorrect-counter 1)))
    (define (is-counter-exceeds-the-limit)
      (if (>= incorrect-counter 7)
	  #t
	  #f))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount)))
    (define (call-the-cops amount)
      "Calling cops")
    (define (dispatch user-passwd m)
      (if (eq? user-passwd passwd)
	  (begin (reset-counter)
		 (cond ((eq? m 'withdraw)
			withdraw)
		       ((eq? m 'deposit)
			deposit)
		       (else (error "MAKE-ACCOUNT" "Unknown request" m))))
	  (begin (inc-counter)
		 (if (is-counter-exceeds-the-limit)
		     call-the-cops
		     (error "MAKE-ACCOUNT" "Password wrong" user-passwd)))))
    dispatch))

;; 使用 monte carlo 测试来计算PI值
;; (define rand
;;   (let ((x random-init))
;;     (lambda ()
;;       (set! x (rand-update x))
;;       x)))
;; 定义以下rand使monte-carlo可以正常运行
(define rand
  (lambda ()
    (random 1000)))
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1)
		 (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1)
		 trials-passed))))
  (iter trials 0))
;; 如果我们没有赋值操作
;; (define (estimate-pi trials)
;;   (sqrt (/ 6 (random-gcd-test trials random-init))))
;; (define (random-gcd-test trials initial-x)
;;   (define (iter trials-remaining trials-passed x)
;;     (let ((x1 (rand-update x)))
;;       (let ((x2 (rand-update x1)))
;; 	(cond ((= trials-remaining 0)
;; 	       (/ trials-passed trials))
;; 	      ((= (gcd x1 x2) 1)
;; 	       (iter (- trials-remaining 1)
;; 		     (+ trials-remaining 1)
;; 		     x2))
;; 	      (else
;; 	       (iter (- trials-remaining 1)
;; 		     trials-passed
;; 		     x2))))))
;;   (iter trials 0 initial-x))

;; Exercise 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (integral-test pred x1 x2 y1 y2)
  (pred (random-in-range x1 x2) (random-in-range y1 y2)))
(define (estimate-integral pred x1 x2 y1 y2 trials)
  (* (- x2 x1) (- y2 y1) (monte-carlo trials
				      (lambda ()
					(integral-test pred x1 x2 y1 y2)))))

(define (square x)
  (* x x))
(define (sample-integral-pred x y)
  (>= (square 3) (+ (square (- x 5))
		    (square (- y 7)))))
(define (estimate-pi-2 trials)
  (/ (estimate-integral sample-integral-pred 2.0 8.0 4.0 10.0 trials)
     (square 3.0)))

;; Exercise 3.6
(define random-init 1)
(define (rand m)
  (let ((x random-init))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset seed)
      (set! x seed))
    (define (dispatch m)
      (cond ((eq? m 'generate)
	     (generate))
	    ((eq? m 'reset)
	     reset)
	    (else
	     (error "RAND" "Unknown message" m))))
    (dispatch m)))

;; The Costs of Introducing Assignment
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

;; 两种factorial
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product) (+ counter 1))))
  (iter 1 1))
(define (factorial-imperative n)
  (let ((product 1)
	(counter 1))
    (define (iter)
      (if (> counter n)
	  product
	  (begin (set! product (* counter product))
		 (set! counter (+ counter 1))
		 (iter))))
    (iter)))

;; Exercise 3.8
(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((password-list (list passwd)))
    (define (add-password new-passwd)
      (set! password-list (cons new-passwd password-list)))
    (define (dispatch user-passwd op)
      (if (memq user-passwd password-list)
	  (cond ((eq? op 'withdraw)
		 withdraw)
		((eq? op 'deposit)
		 deposit)
		((eq? op 'make-joint)
		 add-password)
		(else (error "MAKE-ACCOUNT" "Unknow request" op)))
	  (error "MAKE-ACCOUNT" "Incorrect password" user-passwd)))
    dispatch))
(define (make-joint account passwd new-passwd)
  ((account passwd 'make-joint) new-passwd)
  account)

;; Exercise 3.8
;; 一旦有一次参数为0，则以后总是返回0
;; 否则会返回参数
(define f
  (let ((always-return-zero #f))
    (lambda (n)
      (if always-return-zero
	  0
	  (if (= n 0)
	      (begin (set! always-return-zero #t)
		     0)
	      n)))))

;; Exercise 3.9
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
;; (factorial 6)执行时首先会创建环境E1，
;; E1里有参数n的绑定值6，factorial在全局环境中存在。
;; 由于(factorial 6)内部需要调用(factorial 5)，所以
;; 会创建环境E2，E2执行全局环境，同时包含参数n的绑定值5。
;; 依此类推，随后还会创建环境E3, E4, E5, E6。
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* product counter)
		 (+ counter 1)
		 max-count)))
;; 执行(factorial 6)时首先会创建环境E1，E1指向全局环境，同时保存有n的绑定值6。
;; 随后需要执行(fact-iter 1 1 n)，此时会创建环境E2，E2指向全局环境，同时保存
;; 了三个参数的绑定。然后由于fact-iter内部也需要调用fact-iter，所以又会接着创建
;; 环境E3, E4, E5, E6, E7, E8。

;; Exercise 3.10
;; from: http://wqzhang.wordpress.com/2009/07/13/sicp-exercise-3-10/
;; see https://dl.dropboxusercontent.com/u/63681644/images/sicp-ex-3-10.png

;; Exercise 3.11
;; from: http://wqzhang.wordpress.com/2009/07/14/sicp-exercise-3-11/
;; see https://dl.dropboxusercontent.com/u/63681644/images/sicp-ex-3-11.png

;; cons的实现
;; (define (my-cons x y)
;;   (let ((new (get-new-pair)))
;;     (set-car! new x)
;;     (set-cdr! new y)
;;     new))
