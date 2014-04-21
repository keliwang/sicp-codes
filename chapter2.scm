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
	  (else (error "Argument not 0 or 1: CONS" m)))))
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
