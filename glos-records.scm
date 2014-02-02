;; glos-records.scm

;; (make-record-type 'foo (list super1 ...)
;;    (list 'f1)      -- name 'f1, <top> type, required at construction
;;    (list 'f2 t)    -- name 'f2, t type, required at construction
;;    (list 'f3 t (lambda () exp)))  -- name 'f3, t type, initial value of v
;;                                      from evaluation of exp at constr. time
;; => rec newtype = 
;;      (glos-record-type 'foo
;;         ((field-spec 'f1 <top> #f #f 
;;                      (method ((x newtype)) => <top> ...)
;;                      (method ((x newtype) (y <top>)) ...))
;;           ...  ;; field-specs for fields f2, f3, f4
;;         (method ((f1 <top>) (f2 t)) ...) ;; constructor
;;         (lambda (v) ...))   ;; predicate
;;
;; The option of a field with an initial value but no specified type has 
;; been eliminated to avoid the need for extra parenthesization.

(define (make-record-type name supers . fieldspecs)
  (fill-in-record-type
   (make-glos-record-type name supers '() #f #f)
   fieldspecs
   supers))

;; (defrectype foo (super1 ...)
;;     ;; field specifications:
;;     ((f1) (f2 t) (f3 t v) ...)
;;     ;; field accessor names:
;;     (f1 get-f1 set-f1!)
;;     ... )
(define-syntax defrectype
  (syntax-rules ()
    ((defrectype ?name (?super ...) (?fieldspec ...) ?accessor-spec ...)
     (begin
       (define ?name (make-glos-record-type '?name (list ?super ...) '() #f #f))
       (fill-in-record-type
	?name (collect-glos-fieldspecs (?fieldspec ...) '())
	(list ?super ...))
       (generate-accessor-defines ?name (?accessor-spec ...))
       ?name))))

;; (collect-glos-fieldspecs (fieldspec ...) result)
;; For each fieldspec (name type initexp), 
;;    name is quoted as a symbol, type is evaluated, and 
;;    initexp is put into a thunk (lambda () exp)
(define-syntax collect-glos-fieldspecs
  (syntax-rules ()
    ((collect-glos-fieldspecs ((?name ?type ?initexp) . ?more) ?result)
     (collect-glos-fieldspecs ?more (cons (list '?name ?type (lambda () ?initexp))
					  ?result)))
    ((collect-glos-fieldspecs ((?name ?type) . ?more) ?result)
     (collect-glos-fieldspecs ?more (cons (list '?name ?type) ?result)))
    ((collect-glos-fieldspecs ((?name) . ?more) ?result)
     (collect-glos-fieldspecs ?more (cons (list '?name) ?result)))
    ((collect-glos-fieldspecs () ?result)
     (reverse ?result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;          GLOS-RECORD-TYPE structure
;;
(define-record-type glos-record-type :glos-record-type
  (make-glos-record-type name supers fields constructor predicate)
  glos-record-type?
  (name glos-record-type-name set-glos-record-type-name!)
  (supers glos-record-type-supers)
  (fields glos-record-type-fields set-glos-record-type-fields!)
  (constructor glos-record-type-constructor set-glos-record-type-constructor!)
  (predicate glos-record-type-predicate set-glos-record-type-predicate!))

(define-record-discloser :glos-record-type
  (lambda (v) `(rectype ,(glos-record-type-name v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;          FIELD-SPEC structure
;;
(define-record-type field-spec :field-spec
  (really-make-field-spec name type has-initial-value? initial-value
			  rectype inherited-from/f getter setter)
  field-spec?
  (name field-spec-name)
  (type field-spec-type)
  (has-initial-value? field-spec-has-initial-value?)
  (initial-value field-spec-initial-value)
  (rectype field-spec-rectype)
  (inherited-from/f field-spec-inherited-from/f)
  ;; a method
  (getter field-spec-getter set-field-spec-getter!)
  ;; a method
  (setter field-spec-setter set-field-spec-setter!))

(define-record-discloser :field-spec
  (lambda (v) `(field ,(field-spec-name v)
		      ,(if (field-spec-has-initial-value? v)
			   (field-spec-initial-value v)
			   'no-initial-value))))

;; (define *field-name-spec-table* (make-symbol-table))
(define (make-field-spec name type has-initial-value? initial-value
			 rectype inherited-from/f getter setter)
  (let ((new-spec (really-make-field-spec
		   name type has-initial-value? initial-value
		   rectype inherited-from/f getter setter)))
;     (cond
;      ((table-ref *field-name-spec-table* name)
;       => (lambda (specs)
; 	   (table-set! *field-name-spec-table* name
; 		       (cons new-spec specs))))
;      (else
; 	   (table-set! *field-name-spec-table* name
; 		       (list new-spec))))
    new-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;          INSTANCE structure
;;
(define-record-type instance :instance
  (make-instance type fields)
  instance?
  (type instance-type)
  (fields instance-fields))

(define <object> instance?)

(define-record-discloser :instance
  (lambda (v) `(obj: ,(glos-record-type-name (instance-type v)))))

;; each fieldspec-spec is of one of the following forms:
;;   (name)
;;   (name type) or
;;   (name type initial-value-thunk)
(define (fill-in-record-type new-type fieldspec-specs supertypes)
  (let* ((inherited-fieldspecs (get-inherited-fieldspecs new-type supertypes))
	 (num-fields (+ (length inherited-fieldspecs)
			(length fieldspec-specs)))
	 (new-fieldspecs
	  (let loop ((spec-specs fieldspec-specs)
		     (i (length inherited-fieldspecs))
		     (specs '()))
	    (if (null? spec-specs)
		(reverse specs)
		(let* ((spec-spec (car spec-specs))
		       (name (car spec-spec))
		       (type (if (= 1 (length spec-spec))
				 <top>
				 (cadr spec-spec))))
		  (let ((newspec
			 (make-field-spec
			  name
			  type
			  (= (length spec-spec) 3) ; has-initial-value?
			  (if (= (length spec-spec) 3) ; initial value
			      (list-ref spec-spec 2)
			      #f)
			  new-type	; rectype
			  #f		; inherited-from/f
			  (make-method	; getter
			   (format #f "getter for ~a" name)
			   (make-signature-type #f new-type)
			   type
			   (lambda (obj)
			     (vector-ref (instance-fields obj) i))
			   #f)
			  (make-method	; setter
			   (format #f "setter for ~a" name)
			   (make-signature-type #f new-type type)
			   <top>
			   (lambda (obj newval)
			     (vector-set! (instance-fields obj) i newval))
			   #f))))
		    ;; create generics for accessor methods
		    (make-named-generic 
		     (format #f "getter for field ~a" name)
		     (field-spec-getter newspec))
		    (make-named-generic
		     (format #f "setter for field ~a" name)
		     (field-spec-setter newspec))
		    (loop (cdr spec-specs) (+ 1 i) (cons newspec specs)))))))
	 (fieldspecs (append inherited-fieldspecs new-fieldspecs))
	 (initial-value-thunks
	  (foldr (lambda (fspec vals)
		   (if (field-spec-has-initial-value? fspec)
		       (cons (field-spec-initial-value fspec) vals)
		       vals))
		 '()
		 fieldspecs))
	 (constructor
	  (lambda ()
	    (let* ((field-vec (make-vector num-fields *undefined*))
		   (newobj (make-instance new-type field-vec)))
	      (fold (lambda (fs i)
		      (if (field-spec-has-initial-value? fs)
			  (vector-set! field-vec i 
				       ((field-spec-initial-value fs))))
		      (+ i 1)) 0 fieldspecs)
	      newobj)))
	 (predicate (lambda (v) (and (instance? v)
				     (eq? new-type
					  (instance-type v))))))
    (set-glos-record-type-fields! new-type fieldspecs)
    (set-glos-record-type-constructor! new-type constructor)
    (set-glos-record-type-predicate! new-type predicate)
    new-type))

;; creates new fieldspecs corresponding to inherited fields.
;; updates the generics of the field accessor methods to include the new 
;;    field accessor methods.
(define (get-inherited-fieldspecs new-type supertypes)
  (reverse
   (fold (lambda (super newspecs)
	   (fold (lambda (spec newspecs)
		   (let* ((i (length newspecs))
			  (newspec
			   (make-field-spec
			    (field-spec-name spec)
			    (field-spec-type spec)
			    (field-spec-has-initial-value? spec)
			    (field-spec-initial-value spec)
			    new-type	; rectype
			    super	; inherited-from/f
			    (make-method ; getter
			     (format #f "getter for inherited ~a"
				     (field-spec-name spec))
			     (make-signature-type #f new-type)
			     (field-spec-type spec)
			     (lambda (obj)
			       (vector-ref (instance-fields obj) i))
			     #f)
			    (make-method ; setter
			     (format #f "setter for inherited ~a"
				     (field-spec-name spec))
			     (make-signature-type #f new-type
						  (field-spec-type spec))
			     <top>
			     (lambda (obj newval)
			       (vector-set! (instance-fields obj) i newval))
			     #f))))
		     ;; update generics for super field
		     (let ((gf (method-generic/f (field-spec-getter spec))))
		       (add-primary-method gf (field-spec-getter newspec)))
		     (let ((gf (method-generic/f (field-spec-setter spec))))
		       (add-primary-method gf (field-spec-setter newspec)))
		     (cons newspec newspecs)))
		 newspecs
		 (glos-record-type-fields super)))
	 '()
	 supertypes)))

;; #f if no such field
(define (find-field-with-name name rt)
  (find (lambda (fs)
	  (eq? name (field-spec-name fs)))
	(glos-record-type-fields rt)))

;; errors if no matching field
(define (get-field-by-name! name rt)
  (let ((fspec (find (lambda (fs)
		       (eq? name (field-spec-name fs)))
		     (glos-record-type-fields rt))))
    (assert fspec "Could not find field named ~a in record type ~a"
	    name rt)
    fspec))

;; accessor-accessor is either field-spec-getter or field-spec-setter
(define (get-accessor-generic-by-name rt name accessor-accessor)
  (let* ((fspec (get-field-by-name! name rt))
	 (accessor (accessor-accessor fspec)))
    (assert (method? accessor) "Accessor ~a for field ~a in record type ~a not a method"
	    accessor name rt)
    (assert (method-generic/f accessor) "Accessor method ~a not in a generic."
	    accessor)
    (method-generic/f accessor)))

(define-syntax generate-accessor-defines
  (syntax-rules ()
    ;; getter and setter
    ((generate-accessor-defines
      ?name ((?fieldname ?getter-name ?setter-name) . ?more))
     (begin
       (define ?getter-name (get-accessor-generic-by-name ?name '?fieldname
							  field-spec-getter))
       (define ?setter-name (get-accessor-generic-by-name ?name '?fieldname
							  field-spec-setter))
       ; (check-for-field-name-clashes '?fieldname)
       (generate-accessor-defines ?name ?more)))
    ;; just getter
    ((generate-accessor-defines
      ?name ((?fieldname ?getter-name) . ?more))
     (begin
       (define ?getter-name (get-accessor-generic-by-name ?name '?fieldname
							  field-spec-getter))
       ; (check-for-field-name-clashes '?fieldname)
       (generate-accessor-defines ?name ?more)))
    ;; done
    ((generate-accessor-defines
      ?name ())
     ?name)))

;; If two class def'ns define fields with the same name and the same named 
;; accessor generics, then the second will simply redefine the first, losing
;; the original accesor methods.  We don't have access to binding information, 
;; just field names, so this is just a heuristic warning.  That is, suppose
;; there already has been a (defclass c1 () ((f)) (f get-f set-f!)),
;; then both (defclass c2 () ((f)) (f get-f set-f!)) and
;; (defclass c3 () ((f)) (f get-c3-f set-c3-f!)) will generate warnings from
;; this check, even though only the def'n of c2 actually has the problem.
; (define (check-for-field-name-clashes fieldname)
;   (cond
;    ((table-ref *field-name-spec-table* fieldname)
;     => (lambda (field-specs)
; 	 ;; Look only at non-inherited field-spec's.
; 	 (let ((leaf-fieldspecs (remove field-spec-inherited-from/f field-specs)))
; 	   (if (> (length leaf-fieldspecs) 1)
; 	       (begin
; 		 (format #t "Warning:  duplicate fields named ~s not sharing generics.~%"
; 			 fieldname)
; 		 (format #t "Consider doing
;  (copy-accessors-by-name-from-type-to-type! '~s from-type to-type)~%" fieldname)
; 		 (format #t "where 'to-type' is the most recently defined type.~%"))))))))

; (define (copy-accessors-by-name-from-type-to-type!
; 	 field-name from-type to-type)
;   (cond
;    ((find (lambda (fs)
; 	    (eq? (field-spec-name fs) field-name))
; 	  (glos-record-type-fields from-type))
;     => (lambda (from-field)
; 	 (cond
; 	  ((find (lambda (fs)
; 		   (eq? (field-spec-name fs) field-name))
; 		 (glos-record-type-fields to-type))
; 	   => (lambda (to-field)
; 		(set-generic-methods!
; 		 (field-spec-getter to-field)
; 		 (cons (field-spec-getter from-field)
; 		       (generic-methods (field-spec-getter to-field))))
; 		(set-generic-methods!
; 		 (field-spec-setter to-field)
; 		 (cons (field-spec-setter from-field)
; 		       (generic-methods (field-spec-setter to-field))))))
; 	  (else
; 	   (error "Field ~s not found in to-type ~s.~%"
; 		  field-name to-type)))))
;    (else
;     (error "Field ~s not found in from-type ~s.~%"
; 	   field-name from-type))))

;; eof
