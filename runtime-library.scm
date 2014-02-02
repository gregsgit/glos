;; runtime-library.scm

;; "nonessential" functions for GLOS runtime


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BUILT IN TYPES
;;
;; ***** !!!!! ***** !!!!! *****
;; It better be the case that if I declare type <x> to be
;; (and? <y> P), then the all values that satisfy P better be (isa? <y>)

;; define the following because some of the types (e.g. <complex>) are
;; subtypes (by being and-types).  Thus the programmer knows that if s/he
;; uses the <typename> types, the expected subtyping rules will apply.

(define <boolean> boolean?)
(define <pair> pair?)
(define <symbol> symbol?)
(define <number> number?)
(define <char> char?)
(define <string> string?)
(define <vector> vector?)
(define <port> port?)
(define <procedure> procedure?)

;; TO DO: more/better order to list/pair/null hierarchy
;; Why <list> \not\le <pair>: '() is a list but not a pair.
(define <list> list?)
;; Why <null> \not\le <list>: car and cdr are defined on lists but not '().
;; However, the fact that (list? '()) => #t seems to take precedence.
;;  (and the fact that traditionally lists are sum types of nonempty and empty)
(define <null> (and? <list> null?))

;; TO DO: more/better order to number hierarchy
(define <complex> (and? <number> complex?))
(define <real> (and? <complex> real?))
(define <rational> (and? <real> rational?))
(define <integer> (and? <rational> integer?))

(define <int> <integer>)

(define <cell> cell?)


;; CALLABLES

(define <callable> (or? generic? method? procedure?))

;; method-signature is a convenience function for those methods
;; that have signature-type method-types.  The only methods for which we know 
;; this to be the case are record-subtype constructor methods and slot 
;; accessor methods.
(define (method-signature m)
  (if (signature-type? (method-args-type m))
      (method-args-type m)
      (error "Method ~a does not have a signature-type as its method-args-type"
	     m)))

;; RECORDS

(define <record-type> glos-record-type?)

(define (new rt . args)
  (assert (glos-record-type? rt)
	  "Must call new on a glos-record-type.  Not ~a" rt)
  ;; (1) call make
  (let ((new-obj (apply make rt args)))	; make is a generic function
    ;; (2) call initialize
    (apply initialize new-obj args)
    new-obj))

(defgeneric make
  ;; default method on make ignores args (they are passed to initialize by new)
  (method ((rt <record-type>) :rest ignored-args)
	  ((glos-record-type-constructor rt))))

(defgeneric initialize
  ;; default method treats args as a list of alternating names and values
  (method (obj :rest args)
	  (apply set-by-name* obj args)))
  
(define (get-by-name name val)
  (assert (instance? val) "Value ~a not an instance." val)
  (let ((fspec (get-field-by-name! name (instance-type val))))
    ((field-spec-getter fspec) val)))

(define (set-by-name! name obj newval)
  (let ((fspec (get-field-by-name! name (instance-type obj))))
    ((field-spec-setter fspec) obj newval)))
  
;; args is alternating list of fieldname, value, ...
(define (set-by-name* obj . args)
  (assert (even? (length args))
	  "Invalid name,val,name,val, ... list to set-by-name* ~a" args)
  (let loop ((args args))
    (if (null? args)
	obj
	(begin
	  (set-by-name! (car args) obj (cadr args))
	  (loop (cddr args))))))

;; Whenever encounter a symbol in args, take it as a fieldname, 
;; and invoke the relevant setter, if any, with the next value in args.
;; Does not complain if doesn't find matching field names.
(define (handle-relevant-keys rt obj args)
  (let loop ((args args))
    (if (> (length args) 1)
	(if (symbol? (car args))
	    (cond ((find-field-with-name (car args) rt)
		   => (lambda (field-spec)
			((field-spec-setter field-spec) obj (cadr args))
			(loop (cddr args)))))
	    (loop (cdr args))))))

;; allows 'sym to occur multipe times in args
(define (handle-key sym args handler)
  (let loop ((args args))
    (if (> (length args) 1)
	(if (and (symbol? (car args))
		 (eq? sym (car args)))
	    (begin
	      (handler (cadr args))
	      (loop (cddr args)))
	    (loop (cdr args))))))

(define (describe-constructor rt)
  (let ((arg-types
	 (signature-type-types
	  (method-signature
	   (glos-record-type-constructor rt))))
	(arg-names (foldr (lambda (field names)
			    (if (field-spec-has-initial-value? field)
				names
				(cons (field-spec-name field) names)))
			  '()
			  (glos-record-type-fields rt))))
    (foldr (lambda (name type l)
	     (cons (cons name type) l)) '() arg-names arg-types)))

(define (describe-fields rt)
  (foldr (lambda (field l)
	   (cons (list (field-spec-name field)
		       (field-spec-type field)
		       (if (field-spec-has-initial-value? field)
			   (field-spec-initial-value field)
			   'no-initial-value))
		 l))
	 '()
	 (glos-record-type-fields rt)))

;; CLONE

(defmethod (clone (obj <object>))
  (let* ((obj-type (instance-type obj))
	 (new-obj ((glos-record-type-constructor obj-type))))
    (format #t "created, now setting.~%")
    (for-each (lambda (field)
		((field-spec-setter field) new-obj
		 ((field-spec-getter field) obj)))
	      (glos-record-type-fields obj-type))
    new-obj))

;; eof



; eof
