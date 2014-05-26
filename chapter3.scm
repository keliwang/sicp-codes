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

;; Exercise 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;; (define x (list 'a 'b))
;; (define y (list 'c 'd))
;; (define z (append x y))
;; z
;; => (a b c d)
;; (cdr x)
;; => (b)
;; (define w (append! x y))
;; w
;; => (a b c d)
;; (cdr x)
;; => (b c d)

;; Exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))

;; (last-pair z)将会无限递归下去，因为last-pair的终止
;; 条件是参数的cdr为nil，但是由于z是一个环，它的cdr也依旧
;; 是一个环，不会变成nil，所以last-pair永远都不会停止。

;; Exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))
;; mystery会将其参数(一个list)倒置并返回，同时会将其参数变为只
;; 包含第一个元素的列表。
;; (define v '(a b c))
;; (define w (mystery v))
;; v
;; => (a)
;; w
;; => (c b a)

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)
;; (define x (cons 'a 'b))
;; (define z1 (cons x x))
;; (define z2 (cons '(a b) '(a b)))
;; (set-to-wow! z1)
;; ((wow b) wow b)
;; (set-to-wow! z2)
;; ((wow b) a b)

;; Exercise 3.15
;; from: http://wqzhang.wordpress.com/2009/07/15/sicp-exercise-3-15/
;; see https://dl.dropboxusercontent.com/u/63681644/images/sicp-ex-3-15.png

;; Exercise 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))
(define z1 '(a b c))
(count-pairs z1)
;; => 3
(define x '(a))
(define z2 (list x x))
(count-pairs z2)
;; => 4
(define x '(a))
(define y (cons x x))
(define z3 (cons y y))
(count-pairs z3)
;; => 7
(define z4 '(a b c))
(set-cdr! (cddr z4) z4)
;; (count-pairs z4) ;;无限递归

;; Exercise 3.17
(define (count-pairs lst)
  (let ((visited-pairs '()))
    (define (do-count-pairs lst)
      (if (not (pair? lst))
	  0
	  (if (memq lst visited-pairs)
	      (+ (do-count-pairs (car lst))
		 (do-count-pairs (cdr lst)))
	      (begin (set! visited-pairs (cons lst visited-pairs))
		     (+ (do-count-pairs (car lst))
			(do-count-pairs (cdr lst))
			1)))))
    (do-count-pairs lst)))

;; Exercise 3.18
(define (cycle? lst)
  (let ((unique-pairs '()))
    (define (do-detect-cycle lst)
      (let ((lst-cdr (cdr lst)))
	(if (null? lst-cdr)
	    #f
	    (begin (set! unique-pairs (cons lst unique-pairs))
		   (if (memq lst-cdr unique-pairs)
		       #t
		       (do-detect-cycle lst-cdr))))))
    (do-detect-cycle lst)))

;; Exercise 3.19
;; using http://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare
(define (cycle? lst)
  (define (get-slow lst)
    (if (null? lst)
	'()
	(cdr lst)))
  (define (get-fast lst)
    (cond ((null? lst) '())
	  ((null? (cdr lst)) '())
	  (else (cddr lst))))
  (define (detect-cycle slow fast)
    (cond ((or (null? slow) (null? fast))
	   #f)
	  ((eq? slow fast)
	   #t)
	  (else
	   (detect-cycle (get-slow slow) (get-fast fast)))))
  (detect-cycle (get-slow lst) (get-fast lst)))


(define (my-cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  (else (error "MY-CONS" "Undefined operation" m))))
  dispatch)
(define (my-car z) (z 'car))
(define (my-cdr z) (z 'cdr))

(define (my-cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  ((eq? m 'set-car!) set-x!)
	  ((eq? m 'set-cdr!) set-y!)
	  (else
	   (error "MY-CONS" "Undefined operation" m))))
  dispatch)
(define (my-car z) (z 'car))
(define (my-cdr z) (z 'cdr))
(define (my-set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (my-set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;; Exercise 3.20
;; from http://wizardbook.wordpress.com/2010/12/16/exercise-3-20/
;; see https://dl.dropboxusercontent.com/u/63681644/images/sicp-ex-3-20.png

(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue)
  (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT-QUEUE" "called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE-QUEUE!" "called with an empty queue"))
	(else
	 (set-front-ptr! queue (cdr (front-ptr queue)))	 
	 queue)))

;; Exercise 3.21
(define (print-queue queue)
  (display (front-ptr queue)))

;; Exercise 3.22
(define (make-queue-2)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (print-queue)
      (display front-ptr))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT-QUEUE" "called with an empty queue")
	  (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair)
	       front-ptr)
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)
	       front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE-QUEUE!" "called with empty queue"))
	    (else
	     (set! front-ptr (cdr front-ptr))
	     front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'front-queue)
	     front-queue)
	    ((eq? m 'insert-queue!)
	     insert-queue!)
	    ((eq? m 'delete-queue!)
	     delete-queue!)
	    ((eq? m 'empty-queue?)
	     empty-queue?)
	    (else
	     (error "QUEUE" "unknown message" m))))
    dispatch))

;; Exercise 3.23
(define (make-deque)
  (cons '() '()))
(define (deque-front-ptr deque)
  (car deque))
(define (deque-rear-ptr deque)
  (cdr deque))
(define (deque-set-front-ptr! deque item)
  (set-car! deque item))
(define (deque-set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (empty-deque? deque)
  (and (null? (deque-front-ptr deque))
       (null? (deque-rear-ptr deque))))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT-DEQUE" "called with empty deque")
      (car (deque-front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR-DEQUE" "called with empty deque")
      (car (deque-rear-ptr deque))))
(define (front-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
	   (deque-set-front-ptr! deque new-pair)
	   (deque-set-rear-ptr! deque new-pair))
	  (else
	   (set-cdr! (cdr new-pair) (deque-front-ptr deque))
	   (set-car! (cdr (deque-front-ptr deque)) new-pair)
	   (deque-set-front-ptr! deque new-pair)))))
(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
	   (deque-set-front-ptr! deque new-pair)
	   (deque-set-rear-ptr! deque new-pair))
	  (else
	   (set-car! (cdr new-pair) (deque-rear-ptr deque))
	   (set-cdr! (cdr (deque-rear-ptr deque)) new-pair)
	   (deque-set-rear-ptr! deque new-pair)))))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "FRONT-DELETE-DEQUE!" "called with empty deque"))
	((eq? (deque-front-ptr deque) (deque-rear-ptr deque))
	 (deque-set-front-ptr! deque '())
	 (deque-set-rear-ptr! deque '()))
	(else
	 (deque-set-front-ptr! deque (cddr (deque-front-ptr deque)))
	 (set-car! (cdr (deque-front-ptr deque)) '()))))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "REAR-DELETE-DEQUE!" "called with empty deque"))
	((eq? (deque-front-ptr deque) (deque-rear-ptr deque))
	 (deque-set-front-ptr! deque '())
	 (deque-set-rear-ptr! deque '()))
	(else
	 (deque-set-rear-ptr! deque (cadr (deque-rear-ptr deque)))
	 (set-cdr! (cdr (deque-rear-ptr deque)) '()))))
(define (print-deque deque)
  (define (make-printable-list q)
    (if (null? q)
        '()
        (cons (car q) 
              (make-printable-list (cddr q)))))
  (newline)
  (display (make-printable-list (front-ptr deque))))

;; one dimensional table
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	#f)))
(define (assoc key records)
  (cond ((null? records)
	 #f)
	((equal? key (caar records))
	 (car records))
	(else (assoc key (cdr records)))))
(define (insert-table! key value table)
  (let ((record (assoc key  (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value)
			(cdr table)))))
  'OK)
(define (make-table)
  (list '*table*))

;; two dimensional table
(define (lookup-two-dim key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      #f))
	#f)))
(define (insert-two-dim-table! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable
			(cons (cons key-2 value)
			      (cdr subtable)))))
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
  'OK)

;; local table
(define (make-table-0)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1 (cons key-2 value))
			    (cdr local-table)))))
      'OK)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "TABLE" "Unknown operation" m))))
    dispatch))
(define operation-table (make-table-0))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; Exercise 3.24
(define (make-table-2 same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
	    ((same-key? key (caar records)) (car records))
	    (else
	     (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1 (cons key-1 value))
			    (cdr local-table)))))
      'OK)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "TABLE" "Unknown operation" m))))
    dispatch))

;; Exercise 3.25
(define (make-table-3)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (lookup-table key-list table)
	(let ((subtable (assoc (car key-list) (cdr table))))
	  (if subtable
	      (if (null? (cdr key-list))
		  (cdr subtable)
		  (lookup-table (cdr key-list) (cdr subtable)))
	      #f)))
      (lookup-table key-list local-table))
    (define (insert! key-list value)
      (define (make-entry keys)
	(if (null? (cdr keys))
	    (cons (car keys) value)
	    (list (car keys) (make-entry (cdr keys)))))
      (define (insert-table! key-list table)
	(let ((subtable (assoc (car key-list) (cdr table))))
	  (if subtable
	      (if (null? (cdr key-list))
		  (set-cdr! subtable value)
		  (insert-table! (cdr key-list) subtable))
	      (set-cdr! table
			(cons (make-entry key-list)
			      (cdr table))))))
      (insert-table! key-list local-table)
      'OK)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "TABLE" "Unknown operation" m))))
    dispatch))

;; Exercise 3.26
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((entry-= x (entry set)) set)
	((entry-< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	(else
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))
(define (lookup-tree given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((record (entry set-of-records)))
	(cond ((= given-key (key record))
	       record)
	      ((< given-key (key record))
	       (lookup-tree given-key (left-branch set-of-records)))
	      ((> given-key (key record))
	       (lookup-tree (right-branch set-of-records)))))))
(define (key record)
  (car record))
(define (entry-= x y)
  (key-= (key x) (key y)))
(define (entry-< x y)
  (key-< (key x) (key y)))
(define (entry-> x y)
  (key-> (key x) (key y)))
(define key-= =)
(define key-< <)
(define key-> >)

(define (make-table-4)
  (let ((local-table (list '*table*)))
    (define (lookup key-list) 
      (define (lookup1 keys table)
        (let ((subtable (lookup-tree (car keys) (cdr table))))
          (if subtable
              (if (null? (cdr keys))
                  (cdr subtable)
                  (lookup1 (cdr keys) subtable))
              false)))
      (lookup1 key-list local-table))
    (define (insert! key-list value)
      (define (make-entry keys)
        (if (null? (cdr keys)) 
            (cons (car keys) value)
            (cons (car keys)
                  (make-tree (make-entry (cdr keys))
                             '() '()))))
      (define (insert1 keys table) 
        (let ((subtable (lookup-tree (car keys) (cdr table))))
          (if subtable
              (if (null? (cdr keys))
                  (set-cdr! subtable value)
                  (insert1 (cdr keys) subtable))
              (set-cdr! table (adjoin-set (make-entry keys)
                                          (cdr table))))))
      (insert1 key-list local-table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; Exercise 3.27
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
	     (lookup x table)))
	(or previously-computed-result
	    (let ((result (f x)))
	      (insert-table! x result table)
	      result))))))
(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
	   ((= n 1) 1)
	   (else (+ (memo-fib (- n 1))
		    (memo-fib (- n 2))))))))
;; memo-fib在计算第n个fibonacci数的时候不在
;; 需要重复计算已计算出来的值，那么它所要计算的值
;; 则是(fib 0) -> (fib (- n 1))，因而其复杂度为O(n)。
;; 直接使用(memoize fib)的话，那么它还是要重复计算
;; (fib (- n 1))和(fib (- n 2))。

;; half-adder
;; half-adder
(define (half-adder a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'OK))
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'OK))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'OK)
(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "LOGICAL-NOT" "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
	   (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'OK)
(define (logical-and a b)
  (cond ((and (= a 1) (= b 1)) 1)
	((or (= a 0) (= b 0)) 0)
	(else (error "LOGICAL-AND" "Invalid signals" a b))))

;; Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-gate-procedure)
    (let ((new-value
	   (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-gate-procedure)
  (add-action! a2 or-gate-procedure)
  'OK)
(define (logical-or a b)
  (cond ((and (= a 0) (= b 0)) 0)
	((or (= a 1) (= b 1)) 1)
	(else (error "LOGICAL-OR" "Invalid signals" a b))))

;; Exercise 3.29
(define (compound-or-gate a1 a2 output)
  (define (or-gate-procedure)
    (let ((not-a1 (make-wire))
	  (not-a2 (make-wire))
	  (b (make-wire)))
      (inverter a1 not-a1)
      (inverter a2 not-a2)
      (and-gate not-a1 not-a2 b)
      (inverter b output)))
  (add-action! a1 or-gate-procedure)
  (add-action! a2 or-gate-procedure)
  'OK)
;; 这个or gate实现的延时是一个and gate的延时加上两个inverter的延时

;; Exercise 3.30
(define (ripple-carry-adder a-list b-list s-list c)
  (define (make-carry-list sum-list)
    (if (null? sum-list)
	'()
	(cons (make-wire)
	      (make-carry-list (cdr sum-list)))))
  (define (do-ripple-carray-adder a-list b-list c-in-list s-list c)
    (cond ((null? (cdr c-in-list))
	   (full-adder (car a-list)
		       (car b-list)
		       (car c-in-list)
		       (car s-list)
		       c))
	  (else
	   (full-adder (car a-list)
		       (car b-list)
		       (car c-in-list)
		       (car s-list)
		       (cadr c-in-list))
	   (do-ripple-carray-adder (cdr a-list)
				   (cdr b-list)
				   (cdr c-in-list)
				   (cdr s-list)
				   c))))
  (let ((c-in-list (make-carry-list s-list)))
    (set-signal! (car c-in-list) 0) ;; first c-in is 0
    (do-ripple-carray-adder a-list b-list c-in-list s-list c)))
;; a short version
(define (ripple-carry-adder a-list b-list s-list c)
  (let ((c-in (make-wire)))
    (if (null? (cdr s-list))
	(set-signal! c-in 0)
	(ripple-carry-adder (cdr a-list) (cdr b-list) (cdr s-list) c-in))
    (full-adder (car a-list) (car b-list) c-in (car s-list) c)))
;; delay = n * full-adder-delay
;; = n * (half-adder-delay*2 + or-gate-delay)
;; = n * (2*(max(and-gate-delay+inverter-delay, or-gate-delay)
;;           + and-gate-delay)
;;        + or-gate-delay)

;; wire representation
(define (make-wire)
  (let ((signal-value 0)
	(action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
	    (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "WIRE" "Unknown operation" m))))
    dispatch))
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
	     (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; a sample simulation
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time-agenda the-agenda))
		  action
		  the-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))
(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 (newline)
		 (display name)
		 (display " ")
		 (display (current-time-agenda the-agenda))
		 (display " New-value = ")
		 (display (get-signal wire)))))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

;; implementing the agenda
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda)
  (list 0))
(define (current-time-agenda agenda)
  (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda)
  (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(insert-queue! (segment-queue (car segments))
		       action)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr!
	       segments
	       (cons (make-new-time-segment time action)
		     (cdr segments)))
	      (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
	(set-segments!
	 agenda
	 (cons (make-new-time-segment time action)
	       segments))
	(add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "FIRST-AGENDA-ITEM" "Agenda is empty")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda
			   (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))
(define the-agenda (make-agenda))
