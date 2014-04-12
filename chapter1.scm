;; compute square of x
(define (square x)
  (* x x))

;; compute sum of square a and square b
(define (sum-of-squares a b)
  (+ (square a)
     (square b)))

;; compute abs of x
(define (abs1 x)
  (cond ((< x 0) (- x))
	((= x 0) 0)
	((> x 0) x)))

(define (abs2 x)
  (cond ((< x 0) (- x))
	(else x)))

(define (abs3 x)
  (if (< x 0)
    (- x)
    x))

;; define >= operator
(define (>= x y)
  (not (< x y)))


;; Exercise 1.1
10
;; => 10
(+ 5 3 4)
;; => 12
(- 9 1)
;; => 8
(/ 6 2)
;; => 3
(+ (* 2 4) (- 4 6))
;; => 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
;; => 19
(= a b)
;; => #f
(if (and (> b a) (< b (* a b)))
  b
  a)
;; => 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; => 16
(+ 2 (if (> b a) b a))
;; => 6
(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))
;; => 16

;; Exercise 1.2
(+ 5
   4
   (* 3
      (- 2 (- 3 (+ 6 45)))
      (- 6 2)
      (- 2 7)))

;; Exercise 1.3
(define (sum-of-squares-of-two-larger-ones x y z)
  (cond ((and (> x z) (> y z)) (sum-of-squares x y))
	((and (> x y) (> z y)) (sum-of-squares x z))
	((and (> y x) (> z x)) (sum-of-squares y z))))

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -)
   a
   b))
;; 将操作符当作返回值返回，类似于返回函数对象。

;; Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))
;; (test 0 (p))
;; 解释器会进入假死状态，表明解释器使用的是application order。
;; 如果解释器使用的是normal order的话输出的结果是0。
;; 解释器假死是因为我们定义了p这个无限递归的程序调用，在(test 0 (p))
;; 被执行时，解释器首先需要计算(p)的值，随后便会进入无限递归，导致假死。
;; 而normal order的解释器在完全展开后根本不需要计算(p)的值，因而会正常运行，
;; 输出结果0。

;; Square Root by Newton's Method
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt1 x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    ;(display guess)
    ;(newline)
    ;(sleep (make-time 'time-duration 100000000 0))
    (average guess (/ x guess)))

  (sqrt-iter 1.0 x))

;; Exercise 1.6
(define (new-if predicate
		then-clause
		else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

;; (define (sqrt-iter guess x)
;;   (new-if (good-enough? guess x)
;; 	  guess
;; 	  (sqrt-iter (improve guess) x)))
;; 使用这个new-if来计算sqrt-iter会导致无限递归，
;; new-if作为一个普通的函数，它在执行时需要对它的两个参数都进行求值，
;; 这会导致每次的new-if都会需要计算(sqrt-iter (improve guess x) x)，
;; 而sqrt-iter内又有new-if，这个new-if又需要计算下一个sqrt-iter的值，
;; 从而导致无限递归。而if作为special form则不需要每次都预先执行sqrt-iter
;; 语句，因而不会产生无限递归的状况。

;; Exercise 1.7
;; 对于过小的x值，在计算good-enough?时，对guess的判断则更倾向于常数而不是x。
;; (define (good-enough? guess x)
;;   (< (abs (- (square guess) x)) 0.001))
;; 由上可知，当x小于0.001时，(square guess)更为接近0.001而不是x；同时当x越接近0.001，
;; 它首0.001的影响也越大。
;; 对于过大的值，程序在获得满足条件的值之前可能因为improve计算时的精度问题导致每次产生
;; 的guess值都是其自身。其原因也很直观，当guess越来越接近真实值时，每次improve的量必然越来越小，
;; 当x较大时，guess的小数部分的位数就会减小，从而无法承载非常小的值。
;; 运行(sqrt1 1e13)即可看到具体的情况。
;; 以下是改进算法
(define (sqrt2 x)
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (< (abs (- guess (improve guess x))) 0.00000001))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; 使用let来避免重复调用improve，使用block语法来避免省略参数x
(define (sqrt3 x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess improved-guess)
    (< (abs (- guess improved-guess)) 0.000000000001))
  (define (sqrt-iter guess)
    (let ((improved-guess (improve guess)))
      (if (good-enough? guess improved-guess)
	guess
	(sqrt-iter improved-guess))))
  (sqrt-iter 1.0))
;; 改进后的方案对极小和极大的数都有很好的效果

;; Exercise 1.8
(define (cube-root x)
  (define (improve guess)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))
  (define (good-enough? guess improved-guess)
    (< (abs (- guess improved-guess)) 0.000000000001))
  (define (cube-root-iter guess)
    (let ((improved-guess (improve guess)))
      (if (good-enough? guess improved-guess)
	guess
	(cube-root-iter improved-guess))))
  (cube-root-iter 1.0))

;; linear recursive factorial
(define (factorial1 n)
  (if (= n 1)
    1
    (* n (factorial1 (- n 1)))))

;; linear iterative factorial
(define (factorial2 n)
  (define (iter product counter max-count)
    (if (> counter max-count)
      product
      (iter (* product counter)
		 (+ counter 1)
		 max-count)))
  (iter 1 1 n))

;; chez scheme中trace调用的方法
;; > (trace factorial1)
;; > (factorial1 6)
;; > (untrace factorial1)

;; Exercise 1.9
;; 两个辅助函数
(define (inc n)
  (+ n 1))
(define (dec n)
  (- n 1))

;; 下面使用add来替代+作为函数名
(define (add1 a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))))
(define (add2 a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))
;; 展开(add1 4 5)，recursive
;; (add1 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; 展开(add2 4 5)，iterative
;; (add2 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
