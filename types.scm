;; types.scm

;; TO DO:
;; * limited list typecheck caching:
;;   Want to cache most specific derived list type informatio about lists.
;;   When a list value passes a list-of or list-with predicate, that fact
;;   should be recorded.  Later, when cons, set-car, set-cdr! is done, 
;;   the type of the resulting list can be calculated by doing a lub with types
;;   of new value(s).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TOP TYPE
;;
(define-record-type top-type :top-type
  (make-top-type)
  top-type?)

(define <top> (make-top-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BOTTOM TYPE
;;
(define-record-type bottom-type :bottom-type
  (make-bottom-type)
  bottom-type?)

(define-record-discloser :bottom-type
  (lambda (v) `(bottom-type)))

(define <bottom> bottom-type?)

;; canonical undefined object
(define *undefined* (make-bottom-type))
(define (undefined? obj) (eq? obj *undefined*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EQ TYPES
;;
(define-record-type eq-type :eq-type
  (make-eq-type val)
  eq-type?
  (val eq-type-val))

(define == make-eq-type)

(define <eq-type> eq-type?)

(define-record-discloser :eq-type
  (lambda (v) `(eq-type ,(eq-type-val v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  AND TYPES
;;
(define-record-type and-type :and-type
  (really-make-and-type conjuncts)
  and-type?
  (conjuncts and-type-types))

(define-record-discloser :and-type
  (lambda (v) `(and-type ,@(and-type-types v))))

(define <and-type> and-type?)

;; simple normalization of and-types:
;;   (1) (and x (and y z)) => (and x y z)
;;   (2) (and x y z), (subtype? x y) => (and x z)
;;   (3) (and x) => x
(define (make-and-type . types)
  ;; first get flat list of conjuncts (in reverse order)
  (let ((types1 (fold (lambda (type out)
			(if (and-type? type)
			    (append (reverse (and-type-types type)) out)
			    (cons type out)))
		      '() types)))
    (dbg 'types "make-and-type, types1 = ~a" types1)
    ;; next see if can merge any conjuncts
    (let ((types2 (fold (lambda (type1 out)
			  ;; if any other types are subtypes of type, ignore type
			  (if (any (lambda (type2)
				     (and (not (eq? type1 type2))
					  (subtype? type2 type1)))
				   types1)
			      out
			      (cons type1 out)))
			'() types1)))
      (dbg 'types "make-and-type, types2 = ~a" types2)
      (if (= 1 (length types2))
	  (car types2)
	  (really-make-and-type types2)))))

(define and? make-and-type)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  OR TYPES
;;
(define-record-type or-type :or-type
  (really-make-or-type disjuncts)
  or-type?
  (disjuncts or-type-types))

(define-record-discloser :or-type
  (lambda (v) `(or-type ,@(or-type-types v))))

(define <or-type> or-type?)

;; simple normalization of or-types:
;; (1) (or x (or y z)) => (or x y z)
;; (2) (or x y z), (subtype? x y) => (or y z)
;; (3) (or x) => x
(define (make-or-type . types)
  ;; first get flat list of disjuncts
  (let ((types1 (fold (lambda (type out)
			(if (or-type? type)
			    (append (reverse (or-type-types type)) out)
			    (cons type out)))
		      '() types)))
    (dbg 'types "make-or-type, types1 = ~a" types1)
    ;; next see if can merge any disjuncts
    (let ((types2 (fold (lambda (type1 out)
			  ;; if this is a subtype of any other type, ignore this
			  (if (any (lambda (type2)
				     (and (not (eq? type1 type2))
					  (subtype? type1 type2)))
				   types1)
			      out
			      (cons type1 out)))
			'() types1)))
      (dbg 'types "make-or-type, types2 = ~a" types2)
      (if (= 1 (length types2))
	  (car types2)
	  (really-make-or-type types2)))))

(define or? make-or-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SIGNATURE-TYPES  
;;
;; rest-type/f is element type of each remaining element.
;;   - if rest-type/f is #f, must have exactly (length types) elt.s
(define-record-type signature-type :signature-type
  (really-make-signature-type types rest-type/f)
  signature-type?
  (types signature-type-types)
  (rest-type/f signature-type-rest-type/f))

(define-record-discloser :signature-type
  (lambda (v) `(signature-type ,(signature-type-types v) ':
			       ,(signature-type-rest-type/f v))))

(define (make-signature-type rest-type/f . types)
  (really-make-signature-type types rest-type/f))

(define <signature-type> signature-type?)

;; *covariant*
;; true if any (list) value satisfying sig1 will also satisfy sig2
(define (signature-subtype? sig1 sig2)
  ;; look at arg-types first
  (let ((rest1 (signature-type-rest-type/f sig1))
	(rest2 (signature-type-rest-type/f sig2)))
    (let loop ((arg-types1 (signature-type-types sig1))
	       (arg-types2 (signature-type-types sig2)))
      (cond
       ((null? arg-types1)		; done with arg-types1
	(if (null? arg-types2)	
	    ;; same # of arg-types
	    (if rest1
		(if rest2
		    (subtype? rest1 rest2)
		    #f)			; sig1 has rest, sig2 doesn't
		#t)			; sig1 has no rest.
	    ;; more arg-types2 (but done with arg-types1)
	    (if rest1
		(let loop2 ((arg-types2 arg-types2))
		  (if (null? arg-types2)
		      (if rest2
			  (subtype? rest1 rest2)
			  #f)		; sig1 has rest, sig2 doesn't
		      (if (subtype? rest1 (car arg-types2))
			  (loop2 (cdr arg-types2))
			  #f)))
		#f)))			; more arg-types2 but no more rest1
       ((null? arg-types2)		; more arg1's than arg2's
	(if rest2
	    (let loop2 ((arg-types1 arg-types1))
	      (if (null? arg-types1)
		  (if rest1
		      (subtype? rest1 rest2)
		      #t)
		  (if (subtype? (car arg-types1) rest2)
		      (loop2 (cdr arg-types1))
		      #f)))
	    #f))			; more arg-types1 but no rest2
       (else				; more of both
	(if (subtype? (car arg-types1) (car arg-types2))
	    (loop (cdr arg-types1) (cdr arg-types2))
	    #f))))))
	
(define (vals-match-signature? orig-vals sig)
  (let ((arg-types (signature-type-types sig))
	(rest-type/f (signature-type-rest-type/f sig)))
;     (format #t "  arg-types(~a)=~a, orig-vals(~a)=~a.~%"
; 	    (length arg-types) arg-types (length orig-vals) orig-vals)
;     (format #t "too few: ~a.  too many: ~a~%"
; 	    (< (length orig-vals) (length arg-types))
; 	    (and (not rest-type/f)
; 		 (> (length orig-vals) (length arg-types))))
    ;; quick check 1st:
    (if (or (< (length orig-vals) (length arg-types)) ; not enuf
	    (and (not rest-type/f)
		 (> (length orig-vals) (length arg-types)))) ; too many
	#f
	;; check types
	(let loop ((vals orig-vals)
		   (types arg-types)
		   (in-rest? (null? arg-types)))
	  (cond
	   ((null? vals)		; we already know there are the right #
	    #t)
	   (in-rest?
	    (and (isa? (car vals) rest-type/f)
		 (loop (cdr vals) '() in-rest?)))
	   ((isa? (car vals) (car types))
	    (loop (cdr vals) (cdr types)
		  (null? (cdr types))))
	   (else
	    #f))))))

;; TO DO: make signature-equal? a method
(define (signature-equal? s1 s2)
  ;; (method ((s1 <signature>) (s2 <signature>)) => <boolean>
  (list= eq?
	 (cons (signature-type-rest-type/f s1)
	       (signature-type-types s1))
	 (cons (signature-type-rest-type/f s2)
	       (signature-type-types s2))))

;; try to merge (and together) signature types
(define (glb-signature-types sig-types)
  (assert (not (null? sig-types)) "null sig-types to glb-signature-types")
  (if (= 1 (length sig-types))
      (car sig-types)
      (fold glb-signature-type2 (car sig-types) (cdr sig-types))))

;; think "and" ...
(define (glb-signature-type2 sig-type1 sig-type2)
  (if (or (bottom-type? sig-type1)
	  (bottom-type? sig-type2))
      <bottom>
      ;; shorter first (just to avoid duplicating logic later)
      ;; since glb is symmetric
      (let* ((type1 (if (<= (length (signature-type-types sig-type1))
			    (length (signature-type-types sig-type2)))
			sig-type1
			sig-type2))
	     (type2 (if (<= (length (signature-type-types sig-type1))
			    (length (signature-type-types sig-type2)))
			sig-type2
			sig-type1))
	     (types1 (signature-type-types type1))
	     (types2 (signature-type-types type2))
	     (rest1 (signature-type-rest-type/f type1))
	     (rest2 (signature-type-rest-type/f type2)))
	(let loop ((types1 types1) (types2 types2) (out-types '()))
	  (if (null? types1)		; done with arg-types1
	      (let loop2 ((types2 types2) (out-types out-types))
		(if (null? types2)
		    ;; if either has rest-type = #f, then glb has rest-type = #f
		    (if (or (not rest1) (not rest2))
			(really-make-signature-type out-types #f)
			;; otherwise, rest is glb of rest types
			(let ((rest-glb (types-glb rest1 rest2)))
			  (if (bottom-type? rest-glb)
			      <bottom>
			      (really-make-signature-type out-types rest-glb))))
		    ;; more types in types2
		    (if (not rest1)	; problem
			<bottom>
			(let ((glbtype (types-glb (car types2) rest1)))
			  (if (bottom-type? glbtype)
			      <bottom>
			      (loop2 (cdr types2) (cons glbtype out-types)))))))
	      ;; more types1
	      (let ((glbtype (types-glb (car types1) (car types2))))
		(if (bottom-type? glbtype)
		    <bottom>
		    (loop (cdr types1) (cdr types2) (cons glbtype out-types)))))))))

;; try to merge (or together) signature types
(define (lub-signature-types sig-types)
  (assert (not (null? sig-types)) "null sig-types to lub-signature-types")
  (if (= 1 (length sig-types))
      sig-types
      (fold lub-signature-type2 (car sig-types) (cdr sig-types))))

;; think "or" ...
(define (lub-signature-type2 sig-type1 sig-type2)
  (if (or (top-type? sig-type1)
	  (top-type? sig-type2))
      <top>
      ;; shorter first (just to avoid duplicating logic later)
      ;; since lub is symmetric
      (let* ((type1 (if (<= (length (signature-type-types sig-type1))
			    (length (signature-type-types sig-type2)))
			sig-type1
			sig-type2))
	     (type2 (if (<= (length (signature-type-types sig-type1))
			    (length (signature-type-types sig-type2)))
			sig-type2
			sig-type1))
	     (types1 (signature-type-types type1))
	     (types2 (signature-type-types type2))
	     (rest1 (signature-type-rest-type/f type1))
	     (rest2 (signature-type-rest-type/f type2)))
	(let loop ((types1 types1) (types2 types2) (out-types '()))
	  (if (null? types1)		; done with arg-types1
	      (let loop2 ((types2 types2) (out-types out-types))
		(if (null? types2)
		    (let ((rest-lub (if rest1
					(if rest2
					    (types-lub rest1 rest2)
					    rest1)
					(if rest2
					    rest2
					    #f))))
		      (really-make-signature-type out-types rest-lub))
		    ;; more types in types2
		    (loop2 (cdr types2)
			   (cons (if rest1
				     (types-lub rest1 (car types2))
				     (car types2))
				 out-types))))
	      ;; more types1
	      (loop (cdr types1) (cdr types2)
		    (cons (types-lub (car types1) (car types2))
			  out-types)))))))

;; throw an exception if check fails
(define (check-applicable! type args)
  (if (isa? args type)
      #t
      (error "ck-app!: args ~a don't satisfy method type ~a" args type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; METHOD TYPES
;; 
(define-record-type method-type :method-type
  (really-make-method-type args-type result-type)
  method-type?
  (args-type method-type-args-type)
  (result-type method-type-result-type))

;; common case
(define (make-method-type rest-type/f result-type . arg-types)
  (really-make-method-type
   (really-make-signature-type arg-types rest-type/f)
   result-type))

(define-record-discloser :method-type
  (lambda (v) `(method-type ,(method-type-args-type v) ->
			    ,(method-type-result-type v))))

(define <method-type> method-type?)

(define-syntax ->
  (syntax-rules ()
    ((-> (?arg-type ...) ?result-type)
     (handle-arrow-args
      (?arg-type ...) ?result-type ()))))

(define-syntax handle-arrow-args
  (syntax-rules (:rest)
    ((handle-arrow-args (:rest ?rest-type) ?result-type (?arg-type ...))
     (really-make-method-type
      (really-make-signature-type (reverse (list ?arg-type ...)) ?rest-type)
      ?result-type))
    ((handle-arrow-args () ?result-type (?arg-type ...))
     (really-make-method-type
      (really-make-signature-type (reverse (list ?arg-type ...)) #f)
      ?result-type))
    ((handle-arrow-args (?arg-type . ?more-arg-types) ?result-type ?arg-types)
     (handle-arrow-args ?more-arg-types ?result-type (?arg-type . ?arg-types)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CELL TYPES
;;
(define-record-type cell-type :cell-type
  (cell-of type)
  cell-type?
  (type cell-type-type))

(define-record-discloser :cell-type
  (lambda (v) `(cell-type ,(cell-type-type v))))

(define <cell-type> cell-type?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (subtype-of t)
;;
(define (subtype-of t)
  (rcurry subtype? t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ISA?
;;
(define (isa? val type)
  (cond
   ((eq? <top> type) #t)
   ((eq? val *undefined*) #t)
   ((eq? <bottom> type) #f)
;    ((predicate-subtype? type)
;     ((predicate-subtype-pred type) val))
   ((glos-record-type? type)
    (and (instance? val)
	 (or ((glos-record-type-predicate type) val)
	     (subtype? (instance-type val) type))))
   ((and-type? type)			; conjunction
    (assert (not (null? (and-type-types type))) "Empty and-type in isa?")
    (every (curry isa? val) (and-type-types type)))
   ((or-type? type)			; disjunction
    (assert (not (null? (or-type-types type))) "Empty or-type in is-a?-internal")
    (any  (curry isa? val) (or-type-types type)))
   ((eq-type? type)
    (eq? val (eq-type-val type)))
   ((signature-type? type)
    (cond ((list? val)
	   (vals-match-signature? val type))
	  ((vector? val)
	   (vals-match-signature? (vector->list val) type))
	  (else
	   #f)))
   ((method-type? type)
    (cond ((generic? val)
	   (every (rcurry isa? type) (generic-methods val)))
	  ((method? val)
	   (and
	    ;; use contravariant comparison of arg-types:
	    (subtype? (method-type-args-type type)
		      (method-args-type val))
	    ;; and covariant comparison of result-types:
	    (subtype? (method-result-type val)
		      (method-type-result-type type))))
	  (else
	   #f)))
   ((cell-type? type)
    (cond ((cell? val)
	   (isa? (cell-ref val) (cell-type-type type)))
	  (else
	   #f)))
   ((callable? type)
    (type val))
   (else
    #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SUBTYPE?
;;
;; true if any instance of t1 can be used in a context requiring a t2
;; that is, true if for every value v, (isa? v t1) => (isa? v t2)
;; - note that predicate types are not comparable.
(define (subtype? t1 t2)
  (cond
   ((eq? t1 t2) #t)
   ((eq? t2 <top>) #t)
   ((eq? t1 <top>) #f)
   ((eq? t1 <bottom>) #t)
   ((eq? t2 <bottom>) #f)
;    ((predicate-subtype? t1)
;     (any (rcurry subtype? t2)		; will recurse through all ancestors
; 	 (predicate-subtype-supertypes t1)))
   ;; (and t1_1 ... t1_n) <= t2  iff 
   ;;   1. if t2 = (and t2_1 ... t2_m), t1 <= t2_i for all t2_i.
   ;;   2. else some t1_i <= t2.
   ((and-type? t1)
    (if (and-type? t2)
	(every (curry subtype? t1) (and-type-types t2))
	(any (rcurry subtype? t2) (and-type-types t1))))
   ((or-type? t1);; (or t1_1 ... t1_n) <= t2  iff all t1_i <= t2
    (every (rcurry subtype? t2) (or-type-types t1)))
   ((eq-type? t1);; (eq v) <= t2  iff  v : t2 or t2 : eq(v)
    (or (isa? (eq-type-val t1) t2)
	(and (eq-type? t2) (equal? (eq-type-val t1) (eq-type-val t2)))))
   ((and-type? t2);; t1 <= (and t2_1 ... t2_n) iff t1 <= t2_i for all i
    (every (curry subtype? t1) (and-type-types t2)))
   ((or-type? t2);; t1 <= (or t2_1 ... t2_n) iff t1 <= t2_i for some i
    (any (curry subtype? t1) (or-type-types t2)))
   ((signature-type? t1)
    (and (signature-type? t2)
	 (signature-subtype? t1 t2)))	; covariant
   ((method-type? t1)
    (and (method-type? t2)
	 ;; contravariant in the arg types
	 (subtype? (method-type-args-type t2) (method-type-args-type t1))
	 ;; covariant in result types
	 (subtype? (method-type-result-type t1)
		   (method-type-result-type t2))))
   ((glos-record-type? t1)
    (any (rcurry subtype? t2)
	 (glos-record-type-supers t1)))
   ((cell-type? t1)
    (and (cell-type? t2)
	 (subtype? (cell-type-type t1)
		   (cell-type-type t2))))
   (else
    #f)))

(define <type>
  (make-or-type <bottom> <eq-type> <and-type> <or-type>
		<signature-type> <cell-type> <method-type>
		(lambda (v) (eq? v <top>))))

;		predicate-subtype?))
		     
;; TO DO: throw an exception on error
;; returns val on success
(define (check-type! val type)
  (if (isa? val type)
      val
      (error "check-type! failed: ~a ~a" val type)))

(define <false> (== #f))

(define (false-or t)
  (make-or-type <false> t))

(define (type-equal? t1 t2)
  (or (eq? t1 t2)
      (cond
       ((eq? <top> t1) (eq? <top> t2))
       ((eq? <bottom> t1) (eq? <bottom> t2))
;       ((predicate-subtype? t1) (eq? t1 t2))
       ((glos-record-type? t1) (eq? t1 t2))
       ((and-type? t1) 
	(and (and-type? t2)
	     (= (length (and-type-types t1)) (length (and-type-types t2)))
	     (every type-equal? (and-type-types t1) (and-type-types t2))))
       ((or-type? t1)			; disjunction
	(and (or-type? t2)
	     (= (length (or-type-types t1)) (length (or-type-types t2)))
	     (every type-equal? (or-type-types t1) (or-type-types t2))))
       ((eq-type? t1)
	(and (eq-type? t2)
	     (equal? (eq-type-val t1) (eq-type-val t2))))
       ((signature-type? t1)
	(and (signature-type? t2)
	     (= (length (signature-type-types t1)) (length (signature-type-types t2)))
	     (every type-equal? (signature-type-types t1) (signature-type-types t2))
	     ;; rest-types are either both #f or both equal types
	     (or (and (eq? #f (signature-type-rest-type/f t1))
		      (eq? #f (signature-type-rest-type/f t2)))
		 (type-equal? (signature-type-rest-type/f t1)
			      (signature-type-rest-type/f t2)))))
       ((method-type? t1)
	(and (method-type? t2)
	     (type-equal? (method-type-args-type t1) (method-type-args-type t2))
	     (type-equal? (method-type-result-type t1) (method-type-result-type t2))))
       ((cell-type? t1)
	(and (cell-type? t2)
	     (type-equal? (cell-type-type t1) (cell-type-type t2))))
       (else
	#f))))

(define (types-glb . types)
  (assert (not (null? types)) "null types in types-glb")
  (fold type2-glb <top> types))

(define (type2-glb t glb)
  (cond
   ((subtype? glb t)
    glb)
   ((subtype? t glb)
    t)
   (else
    (make-and-type t glb))))

(define (types-lub . types)
  (assert (not (null? types)) "null types in types-lub")
  (fold type2-lub <bottom> types))

(define (type2-lub t lub)
  (cond
   ((subtype? lub t)
    t)
   ((subtype? t lub)
    lub)
   (else
    (make-or-type t lub))))

; eof
