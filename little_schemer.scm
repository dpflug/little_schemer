(define (atom? a)
  (not (or
	(pair? a)
	(null? a))))

(define (lat? l)
  (cond
   ((null? l) #t)
   ((atom? (car l)) (lat? (cdr l)))
   (else #f)))

(define (member? m l)
  (cond
   ((null? l) #f)
   (or (eq? m (car l))
       (member? m (cdr l)))))

(define (rember a l)
  (cond
   ((null? l) '())
   ((eq? (car l) a) (cdr l))
   (else (cons (car l) (rember a (cdr l))))))

(define (firsts l)
  (cond
   ((null? l) '())
   (else (cons (car (car l)) (firsts (cdr l))))))
