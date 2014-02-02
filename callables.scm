;; callables.scm
;; Definitions of methods, generics, signatures, and the functions that call them.

(define (callable? v)
  (or (generic? v) (method? v) (procedure? v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   GENERIC FUNCTIONS

(define-callable-record-type generic call-generic :generic
  (really-make-generic name methods composer add-method-check/f)
  generic?
  (name generic-name set-generic-name!)
  (methods generic-methods set-generic-methods!)
  ;; composer : (generic list-of-applicable-methods, argvals) => 
  ;;   call-context
  (composer generic-composer set-generic-composer!)
  (add-method-check/f generic-add-method-check/f
		      set-generic-add-method-check/f!))

(define (call-generic callable-gf real-gf . args)
  ;; (1) find applicable methods
  (let ((app-ms
	 (filter (rcurry standard-method-applicable? args)
		 (generic-methods real-gf))))
    ;; (2) compose applicable methods 
    (let ((context
	   ((generic-composer real-gf) real-gf app-ms args)))
      ;; (3) initiate new call context
      (let-fluid *call-context*
		 context
		 (lambda ()
		   ;; (4) and go
		   ((call-context-executor context)))))))

(define-record-discloser :generic
  (lambda (v) `(generic ,(generic-name v))))

(define (make-generic .  methods)
  (apply make-named-generic "anon" methods))

(define (make-named-generic name . methods)
  (let ((gf (really-make-generic name methods
				 primary-composer standard-add-method-check)))
    (for-each (lambda (m)
		(set-method-generic/f! m gf))
	      methods)
    gf))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   METHODS
;;;
(define-callable-record-type method call-method :method
  (make-method name args-type result-type callable generic/f)
  method?
  (name method-name set-method-name!)
  (args-type method-args-type)
  (result-type method-result-type)
  (callable method-callable)
  ;; currently a method can be in at most one generic
  (generic/f method-generic/f set-method-generic/f!))

(define (call-method callable-m real-m . args)
  (dbg 'calls "  call-method - checking sig...")
  ;; TO DO: many times, this check is redundant -- if the method was put
  ;; into an effective fn., it has already shown its applicability!
  (check-applicable! (method-args-type real-m) args) ; throws error if fails
  (dbg 'calls "    sig. checked - doing apply")
  (set-call-context-callable! (fluid *call-context*) callable-m)
  (if (and (method-result-type real-m)
	   (not (top-type? (method-result-type real-m))))
      (check-type! (apply (method-callable real-m) args)
		   (method-result-type real-m))
      (apply (method-callable real-m) args)))

(define-record-discloser :method
  (lambda (v) `(method ,(method-name v) ,(method-args-type v)
		       => ,(method-result-type v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL CONTEXT (reflective interface to dynamic state)
;;;
(define-record-type call-context :call-context
  (make-call-context generic chain next callable argvals executor)
  (generic call-context-generic)
  (chain call-context-chain set-call-context-chain!)
  (next call-context-next set-call-context-next!)
  (callable call-context-callable set-call-context-callable!)
  (argvals call-context-argvals)
  (executor call-context-executor set-call-context-executor!))

(define *call-context* (make-fluid (make-call-context #f #f #f #f #f #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   METHOD COMPOSITION
;;;

;; Choose a single most applicable method, and leave the rest for next-ms
;; Throw exception if not a single most applicable method.
(define (primary-composer generic app-ms vals)
  (if (null? app-ms)
      (error "No applicable method, generic=~a, vals=~a" generic vals)
      (let ((mams (standard-method-selector app-ms vals)))
	(if (> (length mams) 1)
	    (error "Ambiguous: ~a on values ~a" mams vals))
	;; here we know mams is a list of length 1
	(make-call-context
	 generic mams			; generic, chain
	 (remove (curry eq? (car mams)) app-ms) ; next
	 #f vals			; callable, argvals
	 (lambda () (apply (car mams) vals)))))) ; executor
	
;; chain in increasing order
(define (before-composer generic app-ms vals)
  (let ((sorted-ms
	 (reverse			; want general to specific
	  (bubble-sort
	   app-ms
	   (lambda (m1 m2)
	     (subtype? (method-args-type m1)
		       (method-args-type m2)))))))
    (chain-composer generic sorted-ms vals)))

;; chain in decreasing order
(define (after-composer generic app-ms vals)
  (let ((sorted-ms
	 (bubble-sort
	  app-ms
	  (lambda (m1 m2)
	    (subtype? (method-args-type m1)
		      (method-args-type m2))))))
    (chain-composer generic sorted-ms vals)))

;; Composer for a generic with before, after, around, and primary 
;; generic functions.
;; Note that currently, this is identical to after-composer (!)
;; but needs to be a separate object because it's used as the tag
;; to decide if a generic has been "two-leveled".
(define (method-combination-composer . args)
  (apply after-composer args))
	     
; TO DO: *** Concern: next-method from last before method should go to primary ***

(define *return-value* (make-fluid *undefined*))

;; simply execute the applicable methods in order
;;  Note that the fluid variable *return-value*
;;    is bound during execution of an <after> method, if any.
(define (chain-composer generic app-ms vals)
  (make-call-context
   generic app-ms			; generic, chain
   '() #f vals				; next, callable, args
   (lambda ()				; executor
     (fold (lambda (m-todo return-val)
	     (if (eq? <after> (method-args-type m-todo))
		 (let-fluid *return-value*
			    return-val
			    (lambda ()
			      (apply m-todo vals)
			      return-val))
		 (apply m-todo vals)))
	   *undefined*
	   app-ms))))

;; TO DO: call-next-method is too expensive.
;; keeps the current call context, but mutates chain and next
(define (call-next-method)
  (let ((the-context (fluid *call-context*)))
    (cond
     ;; if we're in the midst of a chain, call next method in chain
     ;; ** for now, that means recompose to a new effective fn. **
     ((memq (call-context-callable the-context)
	    (call-context-chain the-context))
      => (lambda (chain-rest)
	   ;; recompose effective function
	   (let ((new-context
		  ((generic-composer (call-context-generic the-context))
		   (call-context-generic the-context)
		   ;; using rest of chain and all next methods
		   (append (cdr chain-rest)
			   (call-context-next the-context))
		   (call-context-argvals the-context))))
	     ;; keep the current context -- hack it
	     (set-call-context-chain!	; chain
	      the-context (call-context-chain new-context))
	     (set-call-context-next!	; next
	      the-context (call-context-next new-context))
	     ;; callable is set by the individual method (in call-method)
	     ;; argvals stay the same
	     (set-call-context-executor! ; executor
	      the-context (call-context-executor new-context))
	     ;; now go
	     ((call-context-executor new-context)))))
     (else
      (error "call-next-method called while not in a chain")))))

(define (unchecked-call callable args)
  (dbg 'calls "unchecked-call ~a(args: ~a)" callable args)
  (cond
   ((method? callable)
    (unchecked-call (method-callable callable) args))
   ((procedure? callable)
    (apply callable args))
   (else
    (error "Invalid callable in unchecked-call: ~a" callable))))

;; Returns most applicable methods (more than one if ambiguous).
(define (standard-method-selector app-meths vals)
  ;; Find most applicable (leaving all others unsorted).
  ;; -- uses only the methods' signatures -- not the actual arg.s.
  (fold
   ;; mams holds candidates for mam (all elts mutually ambiguous)   
   (lambda (m mams)
     (let ((m-type (method-args-type m)))
       ;; Cannot have situation where both m is <= some method m' in mams
       ;; AND m is >= some other function m'' in mams -- that would imply that
       ;; m' and m'' are comparable and therefore not mutually ambiguous. 
       ;; Also, better not have case that arg-types(m) = arg-types(m') as that would
       ;; be duplicate methods problem.
       (cond
	;; if m >= any m' in mams, m is not a mam candidate
	((any (rcurry subtype? m-type)
	      (map method-args-type mams))
	 mams)
	;; if m < any m' in mams, replace all such m' with m
	((any (curry subtype? m-type)
	      (map method-args-type mams))
	 (cons m
	       (filter
		(lambda (m1) 
		  (not (subtype?
			m-type (method-args-type m1))))
		mams)))
	;; otherwise, must be incomparable with all elts of mam, so add m
	(else
	 (cons m mams)))))
   (list (car app-meths))
   (cdr app-meths)))

(define (standard-method-applicable? m vals)
  (isa? vals (method-args-type m)))

;; errs if duplicate
(define (standard-add-method-check m gf)
  (if (find (lambda (m1) 
	      (type-equal? (method-args-type m1)
			   (method-args-type m)))
	    (generic-methods gf))
      (error "Adding duplicate method - use replace-method. ~a, ~a"
	     gf m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;          METHOD COMBINATION
;;

;;  before < primary < after
;;
(define <after> (const #t))
(define <primary> (and? <after> (const #t)))
(define <before> (and? <primary> (const #t)))

;; take plain old generic, with only primary methods, and 
;; replace generic-methods with 3 new "methods", before-method, after-method, 
;; and primary-method.  Each new method has a generic as its callable, and
;; each of these generics has a different composer fn.
;; change composer for original generic to method-combination-composer
(define (make-generic-two-level! gf)
  ;; does this generic need to be abstracted to a two-level generic?
  (if (not (or (eq? (generic-composer gf) method-combination-composer)
	       (eq? (generic-composer gf) around-composer)))
      (begin
	(format #t "making generic ~a two-level~%" gf)
	(let* ((primary-generic
		;; updates method-generic/f ptr.s to new gf
		(apply make-named-generic
		       (format #f "primary generic for ~a" (generic-name gf))
		       (generic-methods gf)))
	       (primary-method
		(make-method (format #f "primary method for ~a"
				     (generic-name gf))
			     <primary> <top> ; args-type, result-type
			     primary-generic gf)) ; callable, generic/f
	       (before-generic
		;; no methods yet
		(really-make-generic
		 (format #f "before generic for ~a" (generic-name gf))
		 '() before-composer	; methods, composer
		 (const #t)))		; add-method: allow dupes
	       (before-method
		(make-method (format #f "before method for ~a"
				     (generic-name gf))
			     <before> <top> ; args-type, result-type
			     before-generic gf)) ; callable, generic/f
	       (after-generic
		;; no methods yet
		(really-make-generic
		 (format #f "after generic for ~a" (generic-name gf))
		 '() after-composer	; methods, composer
		 (const #t)))		; add-method: allow dupes
	       (after-method
		(make-method (format #f "after method for ~a"
				     (generic-name gf))
			     <after> <top> ; args-type, result-type
			     after-generic gf))) ; callable, generic/f
	  (set-generic-methods! gf (list before-method primary-method after-method))
	  (set-generic-composer! gf method-combination-composer)))))

(define (find-hidden-generic gf ref-fn label)
  (cond
   ((eq? (generic-composer gf) around-composer)
    (cond ((find (lambda (m) (eq? (method-args-type m) <default-around-type>))
		 (generic-methods gf))
	   => (lambda (m) (find-hidden-generic (method-callable m) ref-fn label)))
	  (else
	   (error "could not find default around method, finding ~a generic"
		  label))))
   ((eq? (generic-composer gf) method-combination-composer)
    (method-callable (ref-fn (generic-methods gf))))
   (else
    (if (eq? label 'primary)
	gf
	(error "no ~a - generic not abstracted" label)))))

(define (before-generic gf) (find-hidden-generic gf car 'before))
(define (primary-generic gf) (find-hidden-generic gf cadr 'primary))
(define (after-generic gf) (find-hidden-generic gf caddr 'after))

(define (add-primary-method gf m)
  (let ((primary-gf (primary-generic gf)))
    (if (generic-add-method-check/f primary-gf)
	((generic-add-method-check/f primary-gf) m gf))
    (set-method-generic/f! m primary-gf)
    (set-generic-methods!
     primary-gf (cons m (generic-methods primary-gf)))))

;; add-method is synonym for add-primary-method
(define add-method add-primary-method)

(define (add-method* gf . ms)
  (for-each (curry add-primary-method gf)
	    ms))

(define (remove-primary-method gf sig)
  (let ((primary-gf (primary-generic gf)))
    (cond
     ((find (lambda (m1) 
	      (type-equal? sig (method-args-type m1)))
	      (generic-methods primary-gf))
      => (lambda (m)
	   (set-generic-methods!
	    primary-gf
	    (delete! m (generic-methods primary-gf)))))
     (else
      (error "Could not find method matching ~a for generic ~a" sig gf)))))

(define (replace-primary-method gf sig m)
  (remove-primary-method gf sig)
  (add-primary-method gf m))
  
(define replace-method replace-primary-method)

;; remove-method is synonym for remove-primary-method
(define remove-method remove-primary-method)

(define (add-before-method gf m)
  (make-generic-two-level! gf)
  (add-primary-method (before-generic gf)
		      m))

(define (remove-before-method gf sig)
  (remove-primary-method (before-generic gf) sig))

(define (add-after-method gf m)
  (make-generic-two-level! gf)
  (add-primary-method (after-generic gf) m))

(define (remove-after-method gf sig)
  (remove-primary-method (after-generic gf) sig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   AROUND METHODS   (are special)
;;

;; Choose a single most applicable method, and leave the rest for next-ms
;; Throw exception if not a single most applicable method.
;; around-composer is same as primary-composer, but needs separate identity. 
(define (around-composer generic app-ms vals)
  (if (null? app-ms)
      (error "No applicable around method, generic=~a, vals=~a" generic vals)
      (let ((mams (standard-method-selector app-ms vals)))
	(if (> (length mams) 1)
	    (error "Ambiguous: ~a on values ~a" mams vals))
	;; here we know mams is a list of length 1
	(make-call-context
	 generic mams			; generic, chain
	 (remove (curry eq? (car mams)) app-ms) ; next
	 #f vals			; callable, argvals
	 (lambda () (apply (car mams) vals)))))) ; executor

(define <default-around-type> (or? <top> (const #t))) ; will be super of just <top>

;; take a "two-level" generic and turn it into a three-level generic.
(define (make-generic-arounded! gf)
  (make-generic-two-level! gf)
  (if (not (eq? (generic-composer gf) around-composer))
      (begin
	(format #t "making generic aroundable.~%")
	;; default-around will call before-primary-around methods as usual
	(let* ((default-generic		; callable of default around method
		 (really-make-generic
		  (format #f "default around generic for ~a" (generic-name gf))
		  (generic-methods gf)
		  method-combination-composer
		  standard-add-method-check))
	       (default-around-method
		 (make-method
		  (format #f "default around method for ~a" (generic-name gf))
		  <default-around-type> <top> ; args-type, result-type
		  default-generic gf)))	; callable, generic/f
	  (set-generic-methods! gf (list default-around-method))
	  (set-generic-composer! gf around-composer)))))
	     
(define (add-around-method gf m)
  (make-generic-arounded! gf)
  (set-method-generic/f! m gf)
  (set-generic-methods! gf (cons m (generic-methods gf))))

;; eof
