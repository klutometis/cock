(define (wiki-title title)
  #<#EOF
  == #{title}
EOF
)

(define (wiki-parse-docexpr document docexpr)
  (parameterize ((parse-directive wiki-parse-directive)
                 (parse-procedure wiki-parse-procedure)
                 (parse-case-lambda wiki-parse-case-lambda)
                 (parse-parameter wiki-parse-parameter)
                 (parse-scalar wiki-parse-scalar)
                 (parse-syntax wiki-parse-syntax)
                 (parse-read wiki-parse-read)
                 (parse-record wiki-parse-record)
                 (parse-module wiki-parse-module))
    (parse-docexpr document docexpr)))

(define (wiki-parse-docexprs document docexprs)
  (let ((parsed-docexprs (make-stack)))
    (stack-for-each
     docexprs
     (lambda (docexpr)
       (stack-push! parsed-docexprs
                    (wiki-parse-docexpr document docexpr))))
    parsed-docexprs))

(define (wiki-write-docexprs docexprs)
  @("Write the source-derived docexprs as svnwiki."
    (docexprs "The parsed docexprs"))
  (let* ((document (make-document (make-hash-table) (make-stack)))
         (parsed-docexprs (wiki-parse-docexprs document docexprs)))
    (let ((data (document-data document)))
      (write-template
       wiki-preamble
       `((author . ,(hash-table-ref/default data
                                            'author
                                            "Anonymous"))
         (email . ,(hash-table-ref/default data
                                           'email
                                           "anonymous@example.org"))
         (title . ,(hash-table-ref/default data
                                           'title
                                           "Documentation")))))
    (stack-for-each parsed-docexprs (lambda (docexpr) (docexpr)))
    (display wiki-footer)))
