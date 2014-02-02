;; tlet-macro.scm

(define-syntax tlet
  (syntax-rules ()
    ((tlet ?varspecs ?body)
     (gen-tlet-vars ?varspecs ()
		    ?body))))

(define-syntax gen-tlet-vars
  (syntax-rules ()
    ((gen-tlet-vars ((?var ?type ?exp) . ?specs) ?result ?body)
     (gen-tlet-vars ?specs ((?var (check-type! ?exp ?type)) . ?result) ?body))
    ((gen-tlet-vars ((?var ?exp) . ?specs) ?result ?body)
     (gen-tlet-vars ?specs ((?var ?exp) . ?result) ?body))
    ((gen-tlet-vars () ?result ?body)
     (let ?result ?body))))

(define-syntax tlet*
  (syntax-rules ()
    ((tlet* ?varspecs ?body)
     (gen-tlet*-vars ?varspecs ()
		     ?body))))

(define-syntax gen-tlet*-vars
  (syntax-rules ()
    ((gen-tlet*-vars ((?var ?type ?exp) . ?specs) ?result ?body)
     (gen-tlet*-vars ?specs ((?var (check-type! ?exp ?type)) . ?result) ?body))
    ((gen-tlet*-vars ((?var ?exp) . ?specs) ?result ?body)
     (gen-tlet*-vars ?specs ((?var ?exp) . ?result) ?body))
    ((gen-tlet*-vars () ?result ?body)
     (let* ?result ?body))))

;; eof
