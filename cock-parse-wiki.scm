(define (wiki-title title)
  #<#EOF
== #{title}

EOF
)

(define (wiki-subtitle title)
  #<#EOF
=== #{title}

EOF
)

(define (wiki-subsubtitle title)
  #<#EOF
==== #{title}

EOF
)

(define (wiki-subsubsubtitle title)
  #<#EOF
===== #{title}

EOF
)

(define (wiki-source source)
  #<#EOF
<enscript highlight="scheme">#{source}</enscript>

EOF
)

(define (wiki-procedure signature to)
  #<#EOF
<procedure>#{signature} → #{to}</procedure>
EOF
)

;;; What happens with colons in the definition?
(define (wiki-parameter parameter definition)
  #<#EOF
; #{parameter} : #{definition}
EOF
)

(define (wiki-monospace text)
  #<#EOF
{{#{text}}}
EOF
)

(define (wiki-preamble)
  #<#EOF
[[toc:]]

EOF
)

(define (wiki-postamble)
  #<#EOF
Documented by [[/egg/cock|cock]].

EOF
)

;;; Needs to be generalized.
(define (wiki-parse-directive doc expr data document)
  (let ((directive (car doc))
        (arguments (cdr doc))
        (data (document-data document)))
    (case directive
      ((title)
       (let ((title (car arguments)))
         (lambda () (display (wiki-title title)))))
      ((heading)
       (let ((title (car arguments)))
         (lambda ()
           (hash-table-set! data 'heading-level 1)
           (display (wiki-subtitle title)))))
      ((subheading)
       (let ((title (car arguments)))
         (lambda ()
           (hash-table-set! data 'heading-level 2)
           (display (wiki-subsubtitle title)))))
      ;; Shit: we're supporting a different language than LaTeX; TODO:
      ;; intermediate S-expressions over pre-post-order!
      ((subsubheading)
       (let ((title (car arguments)))
         (lambda ()
           (hash-table-set! data 'heading-level 3)
           (display (wiki-subsubsubtitle title)))))
      (else
       (lambda () (warning "wiki-parse-directive -- Unknown directive" directive))))))

(define (wiki-make-heading heading-level)
  (match heading-level
    (0 wiki-subtitle)
    (1 wiki-subsubtitle)
    (2 wiki-subsubsubtitle)
    (3 wiki-subsubsubtitle)))

(define wiki-write-source? (make-parameter #t))

(define (wiki-make-description descriptions)
  (string-join descriptions "\n\n"))

(define (write-wiki-block doc
                          expr
                          data
                          name
                          item
                          . rest-items)
  (let ((heading
         (wiki-make-heading
          (hash-table-ref/default
           data
           'heading-level
           0)))
        (description (wiki-make-description (doc-descriptions doc))))
    (display (heading (wiki-monospace name)))
    (display (string-join (cons item (cons description rest-items)) "\n" 'suffix))
    (when (wiki-write-source?)
      (display (wiki-source (with-output-to-string (lambda () (pp expr))))))))

;;; Generalize this.
(define (make-wiki-procedure template name formals to)
  (template (cons name formals) (string-join to ", ")))

(define (purge-newlines string)
  (irregex-replace/all "\n" string " "))

(define (make-wiki-parameters parameters)
  (let ((parameters
         (map
          (match-lambda ((parameter definition)
                    (wiki-parameter parameter (purge-newlines definition))))
          parameters)))
    (string-join parameters "\n")))

(define (wiki-parse-procedure doc expr data name formals)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (let ((to (tex-procedure-to special-parameters)))
      (let ((procedure
             (make-wiki-procedure wiki-procedure name formals to))
            (parameters
             (make-wiki-parameters normal-parameters)))
        (lambda ()
          (write-wiki-block doc
                            expr
                            data
                            name
                            procedure
                            parameters))))))

(define (wiki-parse-case-lambda doc expr data name formals+)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (let ((to (tex-procedure-to special-parameters)))
      (let ((procedures
             (string-join
              (map (lambda (formals)
                     (make-wiki-procedure
                      wiki-procedure
                      name
                      formals
                      to))
                   formals+)
              "\n"))
            (parameters
             (make-wiki-parameters normal-parameters)))
        (lambda ()
          (write-wiki-block
           doc
           expr
           data
           name
           procedures
           parameters))))))
(define (wiki-parse-docexpr document docexpr)
  (parameterize ((parse-directive wiki-parse-directive)
                 (parse-procedure wiki-parse-procedure)
                 ;; (parse-case-lambda wiki-parse-case-lambda)
                 ;; (parse-parameter wiki-parse-parameter)
                 (parse-case-lambda wiki-parse-case-lambda)
                 ;; (parse-scalar wiki-parse-scalar)
                 ;; (parse-syntax wiki-parse-syntax)
                 ;; (parse-read wiki-parse-read)
                 ;; (parse-record wiki-parse-record)
                 ;; (parse-module wiki-parse-module)
                 )
    (parse-docexpr document docexpr)))

;;; Needs to be generalized.
(define (wiki-parse-docexprs document docexprs)
  (let ((parsed-docexprs (make-stack)))
    (stack-for-each
     docexprs
     (lambda (docexpr)
       (stack-push! parsed-docexprs
                    (wiki-parse-docexpr document docexpr))))
    parsed-docexprs))

;;; Needs to be generalized.
(define (wiki-write-docexprs docexprs)
  @("Write the source-derived docexprs as svnwiki."
    (docexprs "The parsed docexprs"))
  (let* ((document (make-document (make-hash-table) (make-stack)))
         (parsed-docexprs (wiki-parse-docexprs document docexprs)))
    (display (wiki-preamble))
    (stack-for-each parsed-docexprs (lambda (docexpr) (docexpr)))
    (display (wiki-postamble))))
