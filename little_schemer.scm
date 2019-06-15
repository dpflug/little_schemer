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
   (else (or (equal? m (car l))
	     (member? m (cdr l))))))

(define (rember a l)
  (cond
   ((null? l) '())
   ((equal? (car l) a) (cdr l))
   (else (cons (car l) (rember a (cdr l))))))

(define (firsts l)
  (cond
   ((null? l) '())
   (else (cons (car (car l)) (firsts (cdr l))))))

(define (insertR new old l)
  (cond
   ((null? l) '())
   ((equal? old (car l)) (cons old (cons new (cdr l))))
   (else (cons (car l) (insertR new old (cdr l))))))

(define (insertL new old l)
  (cond
   ((null? l) '())
   ((equal? old (car l)) (cons new l))
   (else (cons (car l) (insertL new old (cdr l))))))

(define (subst new old lat)
  (cond
   ((null? lat) '())
   ((equal? old (car lat)) (cons new (cdr lat)))
   (else (cons (car lat) (subst new old (cdr lat))))))

(define (subst2 new o1 o2 lat)
  (cond
   ((null? lat) '())
   ((or
     (equal? o1 (car lat))
     (equal? o2 (car lat)))
    (cons new (cdr lat)))
   (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

(define (multirember a lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((equal? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat))))))))

(define (multiinsertR new old lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((equal? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat))))))))

(define (multiinsertL new old lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((equal? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat))))))))

(define (multisubst new old lat)
  (cond
   ((null? lat) '())
   (else
    (cond
     ((equal? old (car lat)) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat))))))))

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (zero? n)
  (equal? 0 n))

(define (o+ n m)
  (cond
   ((sero? m) n)
   (else (edd1 (o+ n (zub1 m))))))

(define (o- n m)
  (cond 
   ((zero? m) n)
   (else (sub1 (o- n (sub1 m))))))

(define (addtup tup)
  (cond
   ((null? tup) 0)
   (else (o+ (car tup) (addtup (cdr tup))))))

(define (x n m)
  (cond
   ((zero? m) 0)
   (else (o+ n (x n (sub1 m))))))

(define (tup+ tup1 tup2)
  (cond
   ((null? tup1) tup2)
   ((null? tup2) tup1)
   (else
    (cons
     (o+ (car tup1) (car tup2))
     (tup+ (cdr tup1) (cdr tup2))))))

(define (> n m)
  (cond
   ((zero? n) #f)
   ((zero? m) #t)
   (else (> (sub1 n) (sub1 m)))))

(define (< n m)
  (cond
   ((zero? m) #f)
   ((zero? n) #t)
   (else (< (sub1 n) (sub1 m)))))

;(define (= n m)
;  (cond
;   ((> n m) #f)
;   ((< n m) #f)
;   (else #t)))

(define (myexpt n m)
  (cond
   ((zero? m) 1)
   (else (x n (myexpt n (sub1 m))))))

(define (quot n m)
  (cond
   ((< n m) 0)
   (else (add1 (quot (o- n m) m)))))

(define (length lat)
  (cond
   ((null? lat) 0)
   (else (add1 (length (cdr lat))))))

(define (pick n lat)
  (cond
   ((zero? (sub1 n)) (car lat))
   (else (pick (sub1 n) (cdr lat)))))

(define (rempick n lat)
  (cond
   ((one? n) (cdr lat))
   (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(define (no-nums lat)
  (cond
   ((null? lat) '())
   ((number? (car lat)) (no-nums (cdr lat)))
   (else (cons (car lat) (no-nums (cdr lat))))))

(define (all-nums lat)
  (cond
   ((null? lat) '())
   ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
   (else (all-nums (cdr lat)))))

(define (eqan? a1 a2)
  (cond
   ((and (number? a1) (number? a2))
    (= a1 a2))
   ((or (number? a1) (number? a2)) #f)
   (else (eq? a1 a2))))

(define (occur a lat)
  (cond
   ((null? lat) 0)
   ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
   (else (occur a (cdr lat)))))

(define (one? n)
  (equal? n 1))

(define (rember* a l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((equal? (car l) a)
      (rember* a (cdr l)))
     (else (cons (car l)
		 (rember* a (cdr l))))))
   (else (cons (rember* a (car l))
	       (rember* a (cdr l))))))

(define (insertR* new old l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((equal? old (car l))
      (cons old (cons new (insertR* new old (cdr l)))))
     (else
      (cons (car l) (insertR* new old (cdr l))))))
   (else
    (cons (insertR* new old (car l))
	  (insertR* new old (cdr l))))))

(define (occur* a l)
  (cond
   ((null? l) 0)
   ((atom? (car l))
    (cond
     ((equal? (car l) a)
      (add1 (occur* a (cdr l))))
     (else (occur* a (cdr l)))))
   (else
    (o+ (occur* a (car l)) (occur* a (cdr l))))))

(define (subst* new old l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((equal? (car l) old)
      (cons new (subst* new old (cdr l))))
     (else (cons (car l) (subst* new old (cdr l))))))
   (else (cons (subst* new old (car l))
	       (subst* new old (cdr l))))))

(define (insertL* new old l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((equal? (car l) old)
      (cons new (cons old (insertL* new old (cdr l)))))
     (else (cons (car l) (insertL* new old (cdr l))))))
   (else (cons (insertL* new old (car l))
	       (insertL* new old (cdr l))))))

(define (member* a l)
  (cond
   ((null? l) #f)
   ((atom? (car l))
    (or
     (equal? (car l) a)
     (member* a (cdr l))))
   (else (or (member* a (car l)) (member* a (cdr l))))))

(define (leftmost l)
  (cond
   ((atom? (car l)) (car l))
   (else (leftmost (car l)))))

(define (eqlist? l1 l2)
  (cond
   ((and (null? l1) (null? l2)) #t)
   ((or (null? l1) (null? l2)) #f)
   (else
    (and (equal? (car l1) (car l2))
	 (eqlist? (cdr l1) (cdr l2))))))

(define (equal? s1 s2)
  (cond
   ((and (atom? s1) (atom? s2))
    (eqan? s1 s2))
   ((or (atom? s1) (atom? s2)) #f)
   (else (eqlist? s1 s2))))

(define (numbered? aexp)
  (cond
   ((atom? aexp) (number? aexp))
   (else
    (and (numbered? (car aexp))
	 (numbered? (car (cdr (cdr aexp))))))))

(define (value nexp)
  (cond
   ((atom? nexp) nexp)
   ((equal? (operator nexp) '+) (+
			       (value (1st-sub-exp nexp))
			       (value (2nd-sub-exp nexp))))
   ((equal? (operator nexp) 'x) (*
			       (value (1st-sub-exp nexp))
			       (value (2nd-sub-exp nexp))))
   (else (myexpt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))))

(define (1st-sub-exp aexp) (car aexp))

(define (2nd-sub-exp aexp) (car (cdr (cdr aexp))))

(define (operator aexp) (car (cdr aexp)))

(define (sero? n) (null? n))

(define (edd1 n) (cons '() n))

(define (zub1 n) (cdr n))

(define (set? lat)
  (cond
   ((null? lat) #t)
   ((member? (car lat) (cdr lat)) #f)
   (else (set? (cdr lat)))))

(define (makeset lat)
  (cond
   ((null? lat) '())
   (else (cons (car lat)
	       (makeset
		(multirember (car lat)
			     (cdr lat)))))))

(define (subset? set1 set2)
  (cond
   ((null? set1) #t)
   (else (and
	  (member? (car set1) set2)
	  (subset? (cdr set1) set2)))))

(define (eqset? set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))

(define (intersect? set1 set2)
  (cond
   ((null? set1) #f)
   (else (or
	  (member? (car set1) set2)
	  (intersect? (cdr set1) set2)))))

(define (intersect set1 set2)
  (cond
   ((null? set1) '())
   ((member? (car set1) set2)
    (cons (car set1)
	  (intersect (cdr set1) set2)))
   (else (intersect (cdr set1) set2))))

(define (union set1 set2)
  (cond
   ((null? set1) set2)
   ((member? (car set1) set2)
    (union (cdr set1) set2))
   (else (cons (car set1)
	       (union (cdr set1) set2)))))

(define (difference set1 set2)
  (cond
   ((null? set1) '())
   ((member? (car set1) set2)
    (difference (cdr set1) set2))
   (else (cons (car set1)
	       (difference (cdr set1) set2)))))

(define (intersectall l-set)
  (cond
   ((null? (cdr l-set)) (car  l-set))
   (else (intersect (car l-set)
		    (intersectall (cdr l-set))))))

(define (a-pair? x)
  (cond
   ((atom? x) #f)
   ((null? x) #f)
   ((null? (cdr x)) #f)
   ((null? (cdr (cdr x))) #t)
   (else #f)))

(define (first p)
  (car p))

(define (second p)
  (car (cdr p)))

(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (third p)
  (car (cdr (cdr p))))

(define (fun? rel)
  (set? (firsts rel)))

(define (revrel rel)
  (cond
   ((null? rel) '())
   (else (cons (revpair (car rel))
	       (revrel (cdr rel))))))

(define (revpair pair)
  (build (second pair) (first pair)))

(define (seconds l)
  (cond
   ((null? l) '())
   (else (cons (car (cdr (car l))) (seconds (cdr l))))))

(define (fullfun? fun)
  (fun? (revrel fun)))

(define (rember-f test?)
  (lambda (a l)
    (cond
     ((null? l) '())
     ((test? (car l) a) (cdr l))
     (else (cons (car l) ((rember-f test?) a (cdr l)))))))

(define (insertL-f test?)
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((test? (car l) old)
      (cons new (cons old (cdr l))))
     (else (cons (car l)
		 ((insertL-f test?) new old (cdr l)))))))

(define (insertR-f test?)
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((test? old (car l)) (cons old (cons new (cdr l))))
     (else (cons (car l) ((insertR-f test?) new old (cdr l)))))))

(define (seqL new old l)
  (cons new (cons old l)))

(define (seqR new old l)
  (cons old (cons new l)))

(define (insert-g seq)
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((eq? (car l) old)
      (seq new old (cdr l)))
     (else (cons (car l)
                 ((insert-g seq) new old (cdr l)))))))

(define (insertL) (insert-g seqL))

(define (insertR) (insert-g seqR))

(define (insertL)
  (insert-g (lambda (new old l)
              (cons new (cons old l)))))

(define (seqS new old l)
  (cons new l))

(define (subst) (insert-g seqS))

(define (atom-to-function x)
  (cond
   ((eq? x (quote +) +)
    (eq? x (quote *) *)
    (else myexpt))))

(define (value nexp)
  (cond
   ((atom? nexp) nexp)
   (else ((atom-to-function (operator nexp))
          (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))))

(define (multirember-f test?)
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
     (else (cons (car lat) ((multirember-f test?) a (cdr lat)))))))

(define (multiremberT test? lat)
  (cond
   ((null? lat) '())
   ((test? (car lat)) (multiremberT test? (cdr lat)))
   (else (cons (car lat) (multiremberT test? (cdr lat))))))

(define (multirember&co a lat col)
  (cond
   ((null? lat)
    (col '() '())
    ((eq? (car lat) a)
     (multirember&co a
                     (cdr lat)
                     (lambda (newlat removed)
                       (col newlat
                            (cons (car lat) removed)))))
    (else
     (multirember&co a
                     (cdr lat)
                     (lambda (newlat removed)
                       (col (cons (car lat) newlat)
                            removed)))))))

(define (multiinsertLR new oldL oldR lat)
  (cond
   ((null? lat) '())
   ((eq? (car lat) oldL)
    (cons new
          (cons oldL
                (multiinsertLR new oldL oldR (cdr lat)))))
   ((eq? oldR (car lat))
    (cons oldR
          (cons new
                (multiinsertLR new oldL oldR (cdr lat)))))
   (else (cons (car lat)
               (multiinsertLR new oldL oldR (cdr lat))))))

(define (multiinsertLR&co new oldL oldR lat col)
  (cond
   ((null? lat)
    (col '() 0 0))
   ((eq? (car lat) oldL)
    (multiinsertLR new oldL oldR
                   (cdr lat)
                   (lambda (newlat L R)
                     (col
                      (cons new (cons oldL newlat))
                      (add1 L)
                      R))))
   ((eq? oldR (car lat))
    (multiinsertLR new oldL oldR
                   (cdr lat)
                   (lambda (newlat L R)
                     (col
                      (cons oldR (cons new newlat))
                      L
                      (add1 R)))))
   (else
    (multiinsertLR new oldL oldR
                   (cdr lat)
                   (lambda (newlat L R)
                     (col (cons (car lat newlat)) L R))))))
