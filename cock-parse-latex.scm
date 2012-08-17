(define tex-preamble
  "\\documentclass{article}
\\usepackage{fontspec}
\\usepackage{amsmath}
\\usepackage{tabularx}
\\usepackage{minted}
\\usemintedstyle{borland}
\\usepackage[xetex,
  pdfborder=0 0 0,
  colorlinks,
  linkcolor=blue,
  citecolor=blue,
  urlcolor=blue]{hyperref}
\\usepackage{caption}
\\DeclareCaptionType{source}[Source][List of sources]
\\renewenvironment{source}{}{}
\\usepackage{capt-of}
\\title{@TITLE@}
\\author{@AUTHOR@
  \\texttt{<}\\href{mailto:@EMAIL@}
       {\\nolinkurl{@EMAIL@}}\\texttt{>}}
\\begin{document}
\\maketitle
\\tableofcontents
")

(define tex-footer
  "\\end{document}")

(define tex-description
  "\\begin{description}
@ITEMS@
\\end{description}\n")

(define tex-item-description
  "\\item[Description] @DESCRIPTION@")

(define tex-arguments
  "\\emph{@ARGUMENTS@}")

(define tex-procedure
  "\\item[Procedure] \\texttt{@NAME-AND-FORMALS@ $\\to$ @TO@}")

(define tex-scalar
  "\\item[Scalar] \\texttt{@SCALAR@}")

(define tex-parameter
  "\\texttt{@PARAMETER@} & @DEFINITION@")

(define tex-parameters
  "\\item[Parameters]
\\begin{tabularx}{\\textwidth}[t]{lX}
@PARAMETERS@
\\end{tabularx}")

(define tex-parameter-object
  "\\item[Parameter] \\texttt{@PARAMETER@}")

(define tex-syntax
  "\\item[Syntax] \\texttt{@NAME-AND-FORMALS@ $\\to$ @TO@}")

(define tex-read
  "\\item[Read] \\texttt{@FORM@ $\\to$ @TO@}")

(define tex-record
  "\\item[Record] \\texttt{@TYPE@}")

;;; This could be an e.g. function containing a multiline
;;; string-constant with embedded expressions. Sweet!
(define tex-module
  "\\item[Module] \\texttt{@MODULE@}")

(define tex-export
  "\\item \\texttt{@EXPORT@}")

(define tex-exports
  "\\item[Exports] \\hfill
\\begin{itemize}
@EXPORTS@
\\end{itemize}")

(define tex-field
  "\\texttt{@PARAMETER@} & @DEFINITION@")

(define tex-fields
  "\\item[Fields]
\\begin{tabularx}{\\textwidth}[t]{lX}
@PARAMETERS@
\\end{tabularx}")

(define tex-source
  "\\begin{source}
\\begin{minted}[linenos]{scheme}
@SOURCE@\\end{minted}
\\label{@NAME@}
\\end{source}
")

(define tex-case-lambda-procedure
  "\\texttt{@NAME-AND-FORMALS@} & $\\to$ & \\texttt{@TO@}")

(define tex-case-lambda
  "\\item[Procedure] 
\\begin{tabular}[t]{lcl}
@PROCEDURES@
\\end{tabular}")

(define tex-heading
  "\\section{@TITLE@}\n")

(define tex-heading*
  "\\section*{@TITLE@}
\\addcontentsline{toc}{section}{@TITLE@}\n")

(define tex-subheading
  "\\subsection{@TITLE@}\n")

(define tex-subheading*
  "\\subsection*{@TITLE@}
\\addcontentsline{toc}{subsection}{@TITLE@}\n")

(define tex-subsubheading
  "\\subsubsection{@TITLE@}\n")

(define tex-subsubheading*
  "\\subsubsection*{@TITLE@}
\\addcontentsline{toc}{subsubsection}{@TITLE@}\n")

(define tex-substitutions
  '(
    ;; reverts to roman in texttt, etc.; see
    ;; <http://stackoverflow.com/questions/256457/how-does-one-insert-a-backslash-or-a-tilde-into-latex/257624#257624>
    ;; ("\\\\" . "\\textbackslash ")
    ("\\\\" . "\\char`\\\\ ")
    ("\\%" . "\\%")
    ("\\$" . "\\$")
    ("\\{" . "\\{")
    ("\\}" . "\\}")
    ("\\[" . "{[}")
    ("\\]" . "{]}")
    ("\\_" . "\\_")
    ("\\#" . "\\#")
    ("\\^" . "\\^")
    ;; ("\\_" . "{\\textunderscore}")
    ;; ("\n" . " ")
    ("\\~" . "\\~{}")
    ("\\&" . "\\&")
    ;; haven't tried this; is the analog to backslash above
    ;; ("\\~" . "\\char`\\~")
    ))

(define (texify object)
  (string-substitute* (->string object) tex-substitutions #f))

(define (write-tex-block doc
                         expr
                         data
                         name
                         item
                         . rest-items)
  (let ((tex-heading
         (tex-make-heading
          (hash-table-ref/default
           data
           'heading-level
           0)))
        (description
         (tex-make-description (doc-descriptions doc))))
    (write-template
     tex-heading
     'title
     (format "\\texttt{~a}" (texify name)))
    (write-template
     tex-description
     'items
     (string-join
      (cons item (cons description rest-items))
      "\n"))
    (when (write-source?)
      (write-template
       tex-source
       `((source .
                 ,(with-output-to-string
                    (lambda ()
                      (pp expr))))
         (name . ,name))))))

(define (tex-parse-directive doc expr data document)
  (let ((directive (car doc))
        (arguments (cdr doc))
        (data (document-data document)))
    (case directive
      ((email)
       (hash-table-set! data 'email (car arguments))
       void)
      ((author)
       (hash-table-set! data 'author (car arguments))
       void)
      ((title)
       (hash-table-set! data 'title (car arguments))
       void)
      ((heading)
       (lambda ()
         (hash-table-set! data 'heading-level 1)
         (write-template
          tex-heading
          `((title . ,(car arguments))))))
      ((subheading)
       (lambda ()
         (hash-table-set! data 'heading-level 2)
         (write-template
          tex-subheading
          `((title . ,(car arguments))))))
      ((subsubheading)
       (lambda ()
         (hash-table-set! data 'heading-level 3)
         ;; Don't actually have a tex-subsubsheading.
         (write-template
          tex-subheading
          `((title . ,(car arguments))))))
      (else
       (lambda () (warning "tex-parse-directive -- Unknown directive" directive))))))

(define (make-tex-procedure template name formals to)
  (substitute-template
   template
   `((name-and-formals . ,(texify (cons name formals)))
     (to . ,(string-join (map texify to) ", ")))))

(define (make-tex-parameters tex-parameter
                             tex-parameters
                             parameters)
  (let ((parameters
         (map
          (match-lambda
              ((parameter definition)
               (substitute-template
                tex-parameter
                `((parameter . ,(texify parameter))
                  (definition . ,(texify definition))))))
          parameters)))
    (if (null? parameters)
        ""
        (substitute-template
         tex-parameters
         'parameters
         ;; Already texified above.
         (string-join parameters "\\\\\n")))))

(define (tex-parse-procedure doc expr data name formals)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (let ((to (procedure-to special-parameters)))
      (let ((procedure
             (make-tex-procedure tex-procedure name formals to))
            (parameters (make-tex-parameters
                         tex-parameter
                         tex-parameters
                         normal-parameters)))
        (lambda ()
          (write-tex-block
           doc
           expr
           data
           name
           procedure
           parameters))))))

(define (tex-make-heading heading-level)
  (match heading-level
    (0 tex-heading)
    (1 tex-subheading)
    (2 tex-subsubheading)
    ;; Don't have a subsubsubheading.
    (3 tex-subsubheading)))

(define (tex-make-description descriptions)
  (substitute-template
   tex-item-description
   'description
   (string-join (map texify descriptions) "\n\n")))

(define (tex-parse-scalar doc expr data name)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (if (scalar-procedure? normal-parameters special-parameters)
        (tex-parse-procedure doc
                             expr
                             data
                             name
                             (map car normal-parameters))
        (let ((scalar
               (substitute-template
                tex-scalar
                'scalar
                (last expr))))
          (lambda ()
            (write-tex-block
             doc
             expr
             data
             name
             scalar))))))

(define (tex-parse-parameter doc expr data name init)
  (let ((parameter-object
         (substitute-template
          tex-parameter-object
          'parameter
          (texify init))))
    (lambda ()
      (write-tex-block
       doc
       expr
       data
       name
       parameter-object))))

(define (tex-parse-case-lambda doc expr data name formals+)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (let ((to (procedure-to special-parameters)))
      (let* ((procedures
              (string-join
               (map (lambda (formals)
                      (make-tex-procedure
                       tex-case-lambda-procedure
                       name
                       formals
                       to))
                    formals+)
               "\\\\\n"))
             (case-lambda
              (substitute-template
               tex-case-lambda
               'procedures
               procedures)))
        (let ((parameters (make-tex-parameters
                           tex-parameter
                           tex-parameters
                           normal-parameters)))
          (lambda ()
            (write-tex-block
             doc
             expr
             data
             name
             case-lambda
             parameters)))))))

(define (tex-parse-syntax doc expr data name)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (let ((to (procedure-to special-parameters)))
      (let ((syntax
             (make-tex-procedure
              tex-syntax
              name
              (formals normal-parameters)
              to))
            (parameters
             (make-tex-parameters
              tex-parameter
              tex-parameters
              normal-parameters)))
        (lambda ()
          (write-tex-block
           doc
           expr
           data
           name
           syntax
           parameters))))))

(define (tex-parse-read doc expr data char)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (let* ((to (procedure-to special-parameters))
           (read (substitute-template
                  tex-read
                  `((form . ,(texify char))
                    (to . ,(string-join (map texify to) ", "))))))
      (let ((parameters (make-tex-parameters
                         tex-parameter
                         tex-parameters
                         normal-parameters)))
        (lambda ()
          (write-tex-block
           doc
           expr
           data
           char
           read
           parameters))))))

(define (tex-parse-record doc expr data type)
  (receive (normal-parameters special-parameters)
    (doc-normal-and-special-parameters doc)
    (let ((record
           (substitute-template tex-record
                                'type
                                type))
          (fields
           (make-tex-parameters
            tex-field
            tex-fields
            normal-parameters)))
      (lambda ()
        (write-tex-block
         doc
         expr
         data
         type
         record
         fields)))))

(define (make-tex-exports exports)
  (let ((exports (map (lambda (export)
                        (substitute-template
                         tex-export
                         'export
                         (texify export)))
                      exports)))
    (substitute-template tex-exports
                         'exports
                         (string-join exports "\n"))))

(define (tex-parse-module doc expr data name exports)
  (let ((module (substitute-template tex-module
                                     'module
                                     name))
        (exports
         (make-tex-exports
          exports)))
    (lambda ()
      (parameterize ((write-source? #f))
        (write-tex-block
         doc
         expr
         data
         name
         module
         exports)))))

(define (tex-parse-docexpr document docexpr)
  (parameterize ((parse-directive tex-parse-directive)
                 (parse-procedure tex-parse-procedure)
                 (parse-case-lambda tex-parse-case-lambda)
                 (parse-parameter tex-parse-parameter)
                 (parse-scalar tex-parse-scalar)
                 (parse-syntax tex-parse-syntax)
                 (parse-read tex-parse-read)
                 (parse-record tex-parse-record)
                 (parse-module tex-parse-module))
    (parse-docexpr document docexpr)))

(define (tex-parse-docexprs document docexprs)
  (let ((parsed-docexprs (make-stack)))
    (stack-for-each
     docexprs
     (lambda (docexpr)
       (stack-push! parsed-docexprs
                    (tex-parse-docexpr document docexpr))))
    parsed-docexprs))

(define (tex-write-docexprs docexprs)
  @("Write the source-derived docexprs as LaTeX."
    (docexprs "The parsed docexprs"))
  (let* ((document (make-document (make-hash-table) (make-stack)))
         (parsed-docexprs (tex-parse-docexprs document docexprs)))
    (let ((data (document-data document)))
      (write-template
       tex-preamble
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
    (display tex-footer)))
