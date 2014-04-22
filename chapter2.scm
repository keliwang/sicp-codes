;; 加入分数的相关操作，假设已经有了构造分数的工具
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer x))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

;; 实现表示分数的符合数据
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x)
  (car x))
(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

;; Exercise 2.1
;; 更好的处理正负数
(define (make-rat n d)
  (if (negative? d)
    (cons (- n) (- d))
    (cons n d)))

;; Exercise 2.2
(define (average x y)
  (/ (+ x y) 2))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (midpoint-segment segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (make-point (average (x-point start) (x-point end))
		(average (y-point start) (y-point end)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Exercise 2.3
(define (rect-perimeter rect)
  (* (+ (rect-width rect) (rect-height rect)) 2))
(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))
;; 使用相对的两个点，且假设这个长方形边与对于坐标轴平行
(define (make-rect a b)
  (cons a b))
(define (rect-width rect)
  (abs (- (x-point (car rect)) (x-point (cdr rect)))))
(define (rect-height rect)
  (abs (- (y-point (car rect)) (y-point (cdr rect)))))
;; 使用一个顶点和长宽值
(define (make-rect p w h)
  (cons p (cons w h)))
(define (rect-width rect)
  (car (cdr rect)))
(define (rect-height rect)
  (cdr (cdr rect)))

;; 实现cons, car, cdr
(define (my-cons1 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "mycons1" "Argument not 0 or 1: CONS" m))))
  dispatch)
(define (my-car1 z)
  (z 0))
(define (my-cdr1 z)
  (z 1))

;; Exercise 2.4
(define (my-cons2 x y)
  (lambda (m) (m x y)))
(define (my-car2 z)
  (z (lambda (p q) p)))
(define (my-cdr2 z)
  (z (lambda (p q) q)))

;; Exercise 2.5
;; 将(cons x y)用(2^x)*(3^y)来表示，
;; 求car时，我们可以将cons算得的值不断除去2，
;; 直到无法整除为止，统计一共进行了多少次除法
;; 我们就可以获得x的值，同理可以获得y的值。
(define (num-divs n d)
  (define (iter count num)
    (if (= (remainder num d) 0)
      (iter (+ count 1) (/ num d))
      count))
  (iter 0 n))
(define (my-cons3 x y)
  (* (expt 2 x) (expt 3 y)))
(define (my-car3 z)
  (num-divs z 2))
(define (my-cdr3 z)
  (num-divs z 3))

;; Exercise 2.6
;; zero会执行f 0次
;; one会执行f 1次
;; two会执行f 2次
;; 依此类推即可直到这里的用f的可以执行的次数来表示数量
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; interval arithematic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
		   (/ 1.0 (lower-bound y)))))

;; Exercise 2.7
(define (make-interval a b)
  (cons a b))
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))

;; Exercise 2.8
;; [a, b] - [c, d] = [a-d, b-c]
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.9
;; 首先我们看看interval arithematic的加法：
;; [a, b] + [c, d] = [a+c, b+d]
;; 和的宽度为((b+d) - (a+c))/2 = (b-a)/2 + (d-c)/2
;; 由上可知和的宽度等于宽度的和。
;; 乘法和除法不满足的示例：
;; (define a (make-interval 3 5))
;; (define a (make-interval 2 9))
;; (mul-interval a b)
;; => (6 . 45) 其宽度为39/2，很明显不满足
;; (div-interval a b)
;; => (0.33333333333 . 2.5) 其宽度2.166667/2，也不满足
;; 由上知，乘法和除法并不满足。

;; Exercise 2.10
(define (div-interval-with-zero-check x y)
  (let ((upper (upper-bound y))
	(lower (lower-bound y)))
    (if (or (= upper 0) (= lower 0))
      (error "div-interval-with-zero-check" "lower bound and upper bound both cannot be 0" lower upper)
      (mul-interval
	x
	(make-interval (/ 1.0 (upper-bound y))
		       (/ 1.0 (lower-bound y)))))))

;; Exercise 2.11
;; 一个interval的符号只有以下三种情况：[+, +], [-, +], [-, -]
;; 所以两个interval的符号组合至多有3*3=9种。
;; 经过分析，其中只有[-, +], [-, +]这一对必须进行4次乘法运算。
;; 下面的函数里会将0分到+类中
(define (mul-interval x y)
  (define (classify-interval interval)
    (cond ((and (negative? (lower-bound interval))
		(negative? (upper-bound interval)))
	   1) ; [-, -]
	  ((negative? (lower-bound interval))
	   2) ; [-, +]
	  (else 3))) ; [+, +]
  (cond ((and (= (classify-interval x) 1)
	      (= (classify-interval y) 1))
	 (make-interval (* (upper-bound x) (upper-bound y))
			(* (lower-bound x) (lower-bound y))))
	((and (= (classify-interval x) 1)
	      (= (classify-interval y) 2))
	 (make-interval (* (lower-bound x) (upper-bound y))
			(* (lower-bound x) (lower-bound y))))
	((and (= (classify-interval x) 1)
	      (= (classify-interval y) 3))
	 (make-interval (* (lower-bound x) (upper-bound y))
			(* (upper-bound x) (lower-bound y))))
	((and (= (classify-interval x) 2)
	      (= (classify-interval y) 1))
	 (make-interval (* (upper-bound x) (lower-bound y))
			(* (lower-bound x) (lower-bound y))))
	((and (= (classify-interval x) 2)
	      (= (classify-interval y) 2))
	 (let ((p1 (* (lower-bound x) (lower-bound y)))
	       (p2 (* (lower-bound x) (upper-bound y)))
	       (p3 (* (upper-bound x) (lower-bound y)))
	       (p4 (* (upper-bound x) (upper-bound y))))
	   (make-interval (min p1 p2 p3 p4)
			  (max p1 p2 p3 p4))))
	((and (= (classify-interval x) 2)
	      (= (classify-interval y) 3))
	 (make-interval (* (lower-bound x) (upper-bound y))
			(* (upper-bound x) (upper-bound y))))
	((and (= (classify-interval x) 3)
	      (= (classify-interval y) 1))
	 (make-interval (* (upper-bound x) (lower-bound y))
			(* (lower-bound x) (upper-bound y))))
	((and (= (classify-interval x) 3)
	      (= (classify-interval y) 2))
	 (make-interval (* (upper-bound x) (lower-bound y))
			(* (upper-bound x) (upper-bound y))))
	((and (= (classify-interval x) 3)
	      (= (classify-interval y) 3))
	 (make-interval (* (lower-bound x) (lower-bound y))
			(* (upper-bound x) (upper-bound y))))))

;; 使用中间值和容忍度来表示interval
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (average (lower-bound i) (upper-bound i)))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

;; Exercise 2.12
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))
(define (percent i)
  (* 100.0 (/ (width i) (center i))))

;; Exercise 2.13
;; 假设使用中间值和容忍度表示的interval的lower bound都是正值
;; 假设现有两组interval: [a-ap, a+ap], [b-bq, b+bq]
;; [a-ap, a+ap]*[b-bq, b+bq] = [ab-abq-abp+abpq, ab+abq+abp+abpq]
;; 因而其宽度为[(ab+abq+abp+abpq)-(ab-abq-abp+abqp)]/2 = ab(p+q)
;; 其center为：ab(1+pq)
;; 我们可以得出乘积的容忍度为：(p+q)/(1+pq)。
;; 因为p和q都是非常小的值，所以1+pq可以近似看作1，
;; 所以两个interval乘积的容忍度可以用p+q近似表示。

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one
      (add-interval (div-interval one r1)
		    (div-interval one r2)))))

;; Exercise 2.14
;; Lem是对的，我们可以随意测试一下，结果如下
;; (define a (make-center-percent 5 0.8))
;; (par1 a a)
;; => (2.4406349206349205 . 2.560645161290323)
;; (par2 a a)
;; => (2.48 . 2.52)

;; 我们继续测试两组数据
;; (define a (make-center-percent 100 0.001))
;; (define b (make-center-percent 200 0.002))
;; (define aa (div-interval a a))
;; (define ab (div-interval a b))
;; (center aa)
;; => 1.0000000002
;; (percent aa)
;; => 0.001999999999812942
;; (center ab)
;; => 0.5000000003
;; (percent ab)
;; => 0.002999999999402041

;; (define a (make-center-percent 100 40))
;; (define b (make-center-percent 200 50))
;; (define aa (div-interval a a))
;; (define ab (div-interval a b))
;; (center aa)
;; => 1.380952380952381
;; (percent aa)
;; => 68.96551724137932
;; (center ab)
;; => 0.8
;; (percent ab)
;; => 75.00000000000001
;; 从上面两组数据我们可以看到(center a/a)的结果并不是1，
;; 这就明显的说明了我们的计算存在误差，而且percent越大，
;; 误差也越大。
