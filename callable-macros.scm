;; callable-macros.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALLABLE-RECORDS 
;;;

;; If callable record is called with special tag, expose orig. record
;  otherwise, apply (wrapped unwrapped) to caller function.
(define make-record-callable #f)
(define expose-callable-record #f)
(let ((tag (list 'expose-callable-record)))
  (set! make-record-callable
	(lambda (rec caller)
	  (letrec ((callable-fn
		    (lambda args
		      (if (and (= 1 (length args)) (eq? (car args) tag))
			  rec
			  (apply caller callable-fn rec args)))))
	    callable-fn)))
  (set! expose-callable-record
	(lambda (rec)
	  (if (callable-record? rec)
	      (rec tag)
	      rec))))

(define callable-record? #f)
(let* ((t1 (make-record-callable 1 apply))
       (t1-template (closure-template t1))) ; *** SCHEME48 SPECIFIC *** 
  (set! callable-record?
	(lambda (rec)
	  (and (procedure? rec)
	       (eq? (closure-template rec) t1-template)))))

(define-syntax define-callable-record-type
  (syntax-rules ()
    ((define-callable-record-type ?name ?caller ?type ?constructor ?predicate ?field ...)
     (build-callable-redefs
      (?field ...) () ?name ?caller ?type ?constructor ?predicate ?field ...))))

;; redefine field accessors, predicate, and constructor to expose-callable-record 1st
(define-syntax build-callable-redefs
  (syntax-rules ()
    ;; get and set
    ((build-callable-redefs ((?name ?getter ?setter) . ?more) ?result . ?rest)
     (build-callable-redefs
      ?more
      ((let ((orig-getter ?getter)
	     (orig-setter ?setter))
	 (set! ?getter
	       (lambda (r) (orig-getter (expose-callable-record r))))
	 (set! ?setter
	       (lambda (r v) (orig-setter (expose-callable-record r) v))))
       . ?result) . ?rest))
    ;; just get
    ((build-callable-redefs ((?name ?getter) . ?more) ?result . ?rest)
     (build-callable-redefs
      ?more
      ((let ((orig-getter ?getter))
	 (set! ?getter
	       (lambda (r) (orig-getter (expose-callable-record r)))))
       . ?result) . ?rest))
    ;; finish up
    ((build-callable-redefs () ?result
			   ?name ?caller ?type (?constr ?f1 ...) ?predicate ?field ...)
     (begin
       (define-record-type ?name ?type (?constr ?f1 ...) ?predicate ?field ...)
       ;; redefine constructor to do make-record-callable
       (let ((orig-constructor ?constr))
	 (set! ?constr (lambda (?f1 ...)
			 (make-record-callable (orig-constructor ?f1 ...) ?caller))))
       ;; redefine predicate to expose-callable-record 1st
       (let ((orig-predicate ?predicate))
	 (set! ?predicate (lambda (r) (orig-predicate (expose-callable-record r)))))
       . ?result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   GENERIC
;;;
(define-syntax defgeneric
  (syntax-rules ()
    ((defgeneric ?name ?method ...)
     (begin
       (define ?name (make-generic ?method ...))
       (set-generic-name! ?name '?name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   METHOD

(define-syntax method
  (syntax-rules (=>)
    ;; handle result spec., if any
    ((method ?argspec => ?resspec ?body ...) ; handle result spec
     (method-getargs ?argspec (?body ...) ?resspec #f () ()))
    ((method ?argspec ?body ...)	; default result spec is <top>
     (method-getargs ?argspec (?body ...) <top> #f () ()))))

;; not for export
;; (method-getargs argspecs body result restspec args types)
;; collect args and types
(define-syntax method-getargs
  (syntax-rules (:rest)
    ;; handle rest spec, if any
    ((method-getargs (:rest (?rest-var ?rest-type))
		     ?body ?result ?rest ?args ?types)
     (revlstcps ?types ()
		method-finish ?body ?result (?rest-var ?rest-type) ?args))
    ;; rest var. with no specializer:
    ((method-getargs (:rest ?rest-var) ?body ?result ?rest ?args ?types)
     (revlstcps ?types ()
		method-finish ?body ?result (?rest-var <top>) ?args))
    ;; arg. with a specializer:
    ((method-getargs ((?var1 ?type1) ?var2 ...) ?body ?result
		     ?rest (?arg ...) (?type ...))
     (method-getargs (?var2 ...) ?body ?result ?rest (?var1 ?arg ...)
		     (?type1 ?type ...)))
    ;; arg with no specializer - defaults to <top>
    ((method-getargs (?var1 ?var2 ...) ?body ?result ?rest (?arg ...) (?type ...))
     (method-getargs (?var2 ...) ?body ?result ?rest (?var1 ?arg ...)
		     (<top> ?type ...)))
    ;; done with arg.s, no rest
    ((method-getargs () ?body ?result ?rest ?args ?types)
     (revlstcps ?types ()
		method-finish ?body ?result ?rest ?args))
    ))

;; not exported
;; (method-finish body result rest/f args types)
(define-syntax method-finish
  (syntax-rules ()
    ;; no rest
    ((method-finish (?type ...) ?body ?result #f ?args)
     (make-method "anon"
		  (make-signature-type #f ?type ...)
		  ?result
		  (gen-method-lambda ?args () ?body)
		  #f))
    ;; types rest
    ((method-finish (?type ...) ?body ?result (?rest-var ?rest-type) ?args)
     (make-method "anon"
		  (make-signature-type ?rest-type ?type ...)
		  ?result
		  (gen-method-lambda ?args ?rest-var ?body)
		  #f))))

;; not exported
;; reverse the argument list and add rest var, if any
;; (gen-method-lambda args list body)
(define-syntax gen-method-lambda
  (syntax-rules ()
    ((gen-method-lambda (arg1 arg2 ...) l body)
     (gen-method-lambda (arg2 ...) (arg1 . l) body))
    ((gen-method-lambda () l (body ...))
     (lambda l body ...))))

(define-syntax defmethod
  (syntax-rules (=>)
    ((defmethod (?name . ?argspec) ?body ...)
     (begin
       (define ?name (method ?argspec ?body ...))
       (set-method-name! ?name '?name)))
    ((defmethod (?name . ?argspec) => ?result-spec ?body ...)
     (begin
       (define ?name (method ?argspec => ?result-spec ?body ...))
       (set-method-name! ?name '?name)))))

(define-syntax gfmethod
  (syntax-rules (=>)
    ((gfmethod (?name . ?argspec) . ?body)
     (let ((temp-method (method ?argspec . ?body)))
       (set-method-name! temp-method
			 (string->symbol (format #f "method-of-~a" '?name)))
       (add-method ?name temp-method)))
    ((gfmethod (?name . ?argspec) => ?result-spec . ?body)
     (let ((temp-method (method ?argspec => ?result-spec . ?body)))
       (set-method-name! temp-method
			 (string->symbol (format #f "method-of-~a" '?name)))
       (add-method ?name temp-method)))))

;; eof
