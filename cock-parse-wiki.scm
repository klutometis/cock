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

(define (wiki-syntax signature to)
  #<#EOF
<syntax>#{signature} → #{to}</syntax>
EOF
)

(define (wiki-read signature to)
  #<#EOF
<read>#{signature} → #{to}</read>
EOF
)

(define (wiki-record type)
  #<#EOF
<record>#{type}</record>
EOF
)

(define (wiki-record type)
  #<#EOF
<record>#{type}</record>
EOF
)

(define (wiki-module name)
  #<#EOF
'''[module]''' {{#{name}}}

EOF
)

(define (wiki-export export)
  #<#EOF
* [[###{export}]]
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

;; Made these into a strings, because the brackets throw off paredit.
(define (wiki-preamble) "[[toc:]]\n")

(define (wiki-postamble)
  "Documented by [[/egg/cock|cock]].\n")

(define (wiki-parameter-object name init)
  #<#EOF
<parameter>#{name} → #{init}</parameter>
EOF
)

(define (wiki-scalar name definition)
  #<#EOF
<constant>#{name} → #{definition}</constant>
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
      ;; This is where me might to some fancy-schmancy
      ;; markdown-to-wiki bullshit; maybe we can support a subset? I
      ;; really just want monospace and links.
      ((text)
       (let ((text (string-join arguments "\n\n")))
         (lambda ()
           (display text)
           (newline))))
      (else
       (lambda () (warning "wiki-parse-directive -- Unknown directive" directive))))))

(define (wiki-make-heading heading-level)
  (match heading-level
    (0 wiki-subtitle)
    (1 wiki-subsubtitle)
    (2 wiki-subsubsubtitle)
    (3 wiki-subsubsubtitle)))

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
    (when (write-source?)
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
    (let ((to (procedure-to special-parameters)))
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
    (let ((to (procedure-to special-parameters)))
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

(define (wiki-parse-parameter doc expr data name init)
  (let ((parameter (wiki-parameter-object name init)))
    (thunk (write-wiki-block
            doc
            expr
            data
            name
            parameter))))

(define (wiki-parse-scalar doc expr data name)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (if (scalar-procedure? normal-parameters special-parameters)
        (wiki-parse-procedure doc
                              expr
                              data
                              name
                              (map car normal-parameters))
        (let* ((definition (last expr))
               (scalar (wiki-scalar name definition)))
          (thunk (write-wiki-block doc
                                   expr
                                   data
                                   name
                                   scalar))))))

(define (wiki-parse-syntax doc expr data name)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (let ((to (procedure-to special-parameters)))
      (let ((syntax (make-wiki-procedure wiki-syntax
                                         name
                                         (formals normal-parameters)
                                         to))
            (parameters (make-wiki-parameters normal-parameters)))
        (thunk (write-wiki-block doc
                                 expr
                                 data
                                 name
                                 syntax
                                 parameters))))))

(define (wiki-parse-read doc expr data char)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (let* ((to (procedure-to special-parameters))
           (read (wiki-read char (string-join to ", "))))
      (let ((parameters (make-wiki-parameters normal-parameters)))
        (thunk (write-wiki-block doc
                                 expr
                                 data
                                 char
                                 read
                                 parameters))))))

(define (wiki-parse-record doc expr data type)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (let ((record (wiki-record type))
          (fields (make-wiki-parameters normal-parameters)))
      (thunk (write-wiki-block doc
                               expr
                               data
                               type
                               record
                               fields)))))

(define (make-wiki-exports exports)
  (string-join (map wiki-export exports) "\n"))

(define (wiki-parse-module doc expr data name exports)
  (let ((module (wiki-module name))
        (exports (make-wiki-exports exports)))
    (thunk (parameterize ((write-source? #f))
             (write-wiki-block doc
                               expr
                               data
                               name
                               module
                               exports)))))

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
