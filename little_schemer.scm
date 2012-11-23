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

(define (insertR new old l)
  (cond
   ((null? l) '())
   ((eq? old (car l)) (cons old (cons new (cdr l))))
   (else (cons (car l) (insertR new old (cdr l))))))

(define (insertL new old l)
  (cond
   ((null? l) '())
   ((eq? old (car l)) (cons new l))
   (else (cons (car l) (insertL new old (cdr l))))))

(define (subst new old lat)
  (cond
   ((null? lat) '())
   ((eq? old (car lat)) (cons new (cdr lat)))
   (else (cons (car lat) (subst new old (cdr lat))))))

(define (subst2 new o1 o2 lat)
  (cond
   ((null? lat) '())
   ((or
     (eq? o1 (car lat))
     (eq? o2 (car lat)))
    (cons new (cdr lat)))
   (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

(define (multirember a lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat))))))))

(define (multiinsertR new old lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat))))))))

(define (multiinsertL new old lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat))))))))

(define (multisubst new old lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat))))))))

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (zero? n)
  (eq? 0 n))

(define (o+ n m)
  (cond
   ((zero? m) n)
   (else (add1 (o+ n (sub1 m))))))

(define (o- n m)
  (cond 
   ((zero? m) n)
   (else (sub1 (o- n (sub1 m))))))
