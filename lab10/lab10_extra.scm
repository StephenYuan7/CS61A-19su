;; Scheme ;;


(define lst
  (cons (list 1) (cons 2 (cons (list 3 4) (list 5))))
)

(define (composed f g)
  (define (helper x) (f (g x)))
  helper
)

(define (remove item lst)
  (if (null? lst) nil
    (if (= (car lst) item)
        (remove item (cdr lst))
        (cons (car lst) (remove item (cdr lst)))
    )
  )
)


;;; Tests
(remove 3 nil)
; expect ()
(remove 3 '(1 3 5))
; expect (1 5)
(remove 5 '(5 3 5 5 1 4 5 4))
; expect (3 1 4 4)

(define (no-repeats s)
  (if (null? s)
    nil
    (cons (car s) (no-repeats (filter (lambda (x) (not (= (car s) x))) (cdr s))))
  )
)

(define (substitute s old new)
  (if (null? s)
    nil
    (cons
      (if (not (pair? (car s)))
        (if (equal? (car s) old)
          new
          (car s))
        (substitute (car s) old new))
      (substitute (cdr s) old new))))


(define (sub-all s olds news)
    (if (null? olds)
        s
        (sub-all (substitute s (car olds) (car news)) (cdr olds) (cdr news))
    )
)