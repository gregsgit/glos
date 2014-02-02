;	$Id: utils.scm,v 1.9 2000/03/21 20:04:34 gregs Exp gregs $	

;; Some generally useful functions, IMHO.  YMMV.

(define identity (lambda (x) x))

(define foldr fold-right)

(define (more? l) (not (null? l)))

(define (spaces i)
  (do ((i i (- i 1)) (out '() (cons #\space out)))
      ((<= i 0) (list->string out))))

(define (assert test . args)
  (if (not test)
      (apply error args)))

(define (bubble-sort l le?)
  (if (null? l) l
      ;; invariant: out is sorted
      (let loop ((in (cdr l))
		 (out (list (car l))))
	(if (null? in)
	    out
	    (let ((elt (car in)))
	      (if (le? elt (car out))
		  (loop (cdr in) (cons elt out))
		  ;; destructively insert elt into out
		  (let inner ((out-ptr out))
		    (cond
		     ((null? (cdr out-ptr))
		      (set-cdr! out-ptr (cons elt '()))
		      (loop (cdr in) out))
		     ((le? elt (cadr out-ptr))
		      (set-cdr! out-ptr (cons elt (cdr out-ptr)))
		      (loop (cdr in) out))
		     (else
		      (inner (cdr out-ptr)))))))))))

(define curry
  (lambda (f . largs)
    (lambda rargs
      (apply f (append largs rargs)))))

(define rcurry
  (lambda (f . rargs)
    (lambda largs
      (apply f (append largs rargs)))))

(define (const k)
  (lambda ignore k))

;; f must be a function of one argument
(define (compose f g)
  (lambda args
    (f (apply g args))))

;; ex: (filter-collecting 
;;       (lambda (x y) (< x y)
;;       (lambda (x y) (+ x y)
;;       '(1 7 3 9)
;;       '(5 5 5 5)
;;  => (6 8)
(define (filter-collecting predicate collector . lists)
  (let loop ((lists lists) (out '()))
    (if (null? (car lists))
	(reverse out)
	(let ((heads (map car lists)))
	  (if (apply predicate heads)
	      (loop (map cdr lists) (cons (apply collector heads) out))
	      (loop (map cdr lists) out))))))

;; return all but the first n elements of in-lst
;; Ex: (not-first-n 2 '(a b c d)) => '(c d)
(define (not-first-n n in-lst)
  (let loop ((lst in-lst) (i 0))
    (cond
     ((>= i n) lst)
     ((null? lst) (error "not-first-n, n=~a, past end of list ~a" n in-lst))
     (else (loop (cdr lst) (+ i 1))))))

(define (last l)
  (car (reverse l)))

(define (non-last l)
  (reverse (cdr (reverse l))))

(define (corresponding v l1 l2)
  (let loop ((l1 l1) (l2 l2))
    (if (null? l1)
	#f
	(if (eq? v (car l1))
	    (car l2)
	    (loop (cdr l1) (cdr l2))))))

;; return elts from l1 that are not also in l2
(define (set-difference l1 l2)
  (filter (lambda (elt)
	    (if (memq elt l2) #f #t))
	  l1))

;; returns a list of subsets (as lists) of l
(define (powerset-of l)
  (let loop ((l l)
	     (out (list '())))
    (if (null? l)
	out
	(loop (cdr l)
	      (append out
		      (map (curry cons (car l))
			   (powerset-of (cdr l))))))))


;; true if every elt of l1 is also an elt of l2
(define (subseteq? l1 l2)
  (every (lambda (elt1) (memq elt1 l2)) l1))

(define (symbols-to-string syms)
  (do ((syms syms (cdr syms))
       (str "" (string-append str (symbol->string (car syms)))))
      ((null? syms) str)))

;; Remove 1st occurrence of s2, if any, from s1.
(define (string-remove s1 s2)
  (let ((s1-len (string-length s1))
	(s2-len (string-length s2)))
    ;; remember substring(s i j) is s from i (inclusive) up to j (exclusive)
    (let loop ((i 0) (j s2-len))
      (if (> i (- s1-len s2-len))
	  s1				; not found -- return s1
	  (if (string=? (substring s1 i j) s2)
	      (string-append (substring s1 0 i)
			     (substring s1 j s1-len))
	      (loop (+ i 1) (+ j 1)))))))

(define (make-name format-string . args)
  (string->symbol (apply format #f format-string args)))

(define (looks-like-an-integer? e)
  (let* ((s (format #f "~a" e))
	 (l (string->list s)))
    (every char-numeric? l)))

(define *gensym-counter* (vector 0))
(define gensym (lambda ()
		 (let ((n (vector-ref *gensym-counter* 0)))
		   (vector-set! *gensym-counter* 0 (+ n 1))
		   (+ n 1))))
			      
(define (not-false v) (if v #t #f))

(define maybe-err
  (lambda format-args
    (apply format #t format-args)
    (format #t "  break?[y to break]")
    (if (eq? 'y (read))
	(apply error format-args)
	#t)))

(define *dbg-tags* '())

(define dbg
  (lambda (tag format-string . format-args)
    (if (memq tag *dbg-tags*)
	(begin
	  (format #t "[dbg:~a:" tag)
	  (apply format #t format-string format-args)
	  (format #t ":~a:dbg]~%" tag)))))

;; eof
