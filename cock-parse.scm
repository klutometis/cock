(module cock-parse
  (parse-file
   tex-write-docexprs)
  (import chicken
          data-structures
          extras
          ports
          scheme
          srfi-1
          stack)
  (use alist-lib
       ;; Do we want cock here? Does it matter? When called as a
       ;; module, what's the relationship to -X cock? I'm including
       ;; it, and thus need to (use cock) (include "cock-parse").
       ;;
       ;; http://srfi.schemers.org/srfi-17/srfi-17.html and
       ;; http://srfi.schemers.org/srfi-61/srfi-61.html are cool, by
       ;; the way: arbitrary lvalues in set! and (<generator> <guard>
       ;; => <receiver>) in cond.
       ;;
       ;; Cock, I think, is irrelevant here: it needs to be -X cock or
       ;; we need to parse this with cock-parse.
       cock
       debug
       define-record-and-printer
       lolevel
       matchable
       regex
       srfi-13
       srfi-69
       stack)

  (import-for-syntax matchable)

  (define current-docexpr
    @("Enables communication with the parsing @-reader")
    (make-parameter #f))

  (define docexprs (make-parameter (make-stack)))

  (define-record-and-printer null-expression)
  (define null-expression (make-null-expression))

  (set-read-syntax! #\@
    (lambda (in)
      (let ((expression (read in)))
        (cond ((symbol? expression)
               (prepend-@ expression))
              ((pair? expression)
               (current-docexpr expression)
               (stack-push! (docexprs)
                            (make-docexpr (current-docexpr)
                                          null-expression))
               (values))
              (else expression)))))

  (define-record-and-printer docexpr
    doc
    expr)

  (define parse-directive (make-parameter void))

  (define parse-procedure (make-parameter void))

  (define parse-scalar (make-parameter void))

  (define parse-parameter (make-parameter void))

  (define parse-case-lambda (make-parameter void))

  (define parse-syntax (make-parameter void))

  ;; Somehow, we have to process these preamble-directives before we
  ;; spit the document out; could it be that we have to keep the thing
  ;; in memory before we spit it out?
  ;;
  ;; The document has some header fields and a list of docexprs: thus,
  ;; we can process the docexprs in order, pushing to the section
  ;; stack; &c.
  ;;
  ;; Should we say, more formally, that directives are things which
  ;; work on the document; and have first-class support for things like
  ;; sections?
  ;;
  ;; Sections, &c. could work, I suppose, by pushing something unto the
  ;; docexprs stack.
  ;;
  ;; It's a shame, though, that the document-fields are fixed; and that
  ;; directives don't have the ability to put arbitrary data in there.
  ;; Why not add a hash-table called data?
  ;;
  ;; The idea is that the renderers check for some kind of field in the
  ;; hash-table, supplying a reasonable default.
  ;;
  ;; It's a shame, though, that we have to special case so-called
  ;; directives; can every parsed docexpr work on the document?
  ;;
  ;; Non-directive docexprs would have to push themselves on the
  ;; docexpr-stack, though.
  ;;
  ;; Why not push every docexpr on the stack and convert the directives
  ;; into no-ops? Bingo.
  ;;
  ;; docexprs are lambdas: at construction time, they take a document they
  ;; can modify. At invocation time, they write something.
  ;;
  ;; If we were to support more than latex, though, how would they know
  ;; to dispatch? Do we need an e.g. write-docexpr-as-{html,latex},
  ;; such that we need to maintain the types?
  ;;
  ;; If I go with the dispatch-on-type, though, I have to come up with
  ;; types for e.g. headings and subheading; which is a pain in the
  ;; ass. Oh, for pure lambdas!
  (define-record-and-printer document
    data
    docexprs)

  (define (parse-docexpr document docexpr)
    (let ((doc (docexpr-doc docexpr))
          (expr (docexpr-expr docexpr))
          (data (document-data document)))
      (match expr
        ((? null-expression?)
         ((parse-directive) doc expr data document))
        (('define (procedure . formals) . body)
         ((parse-procedure) doc expr data procedure formals))
        (('define procedure ('lambda formals . body))
         ((parse-procedure) doc expr data procedure formals))
        (('define procedure ('case-lambda (formals . body) ...))
         ((parse-case-lambda) doc expr data procedure formals))
        (('define parameter ('make-parameter init . converter))
         ((parse-parameter) doc expr data parameter init))
        (('define scalar . body)
         ((parse-scalar) doc expr data scalar))
        (('define-syntax name . _)
         ((parse-syntax) doc expr data name))
        (_ 'unknown))))

  (define substitute-template
    (case-lambda
     ((template key substitution)
      (substitute-template template `((,key . ,substitution))))
     ((template substitutions)
      (string-substitute*
       template
       (map
        (match-lambda ((key . value)
                  (cons
                   (format "@~a@" (string-upcase (symbol->string key)))
                   (->string value))))
        substitutions)
       #f)))) 

  (define (write-template . keys-or-substitutions)
    (display (apply substitute-template keys-or-substitutions)))

  (define special-parameters '(@to))

  (define (special-parameter? parameter)
    (memq parameter special-parameters))

  (define normal-parameter? (complement special-parameter?))

  (define (doc-descriptions doc)
    (filter string? doc))

  (define (doc-normal-and-special-parameters doc)
    (let ((parameters (filter pair? doc)))
      (let ((normal-parameters
             (filter (compose normal-parameter? car) parameters))
            (special-parameters
             (filter (compose special-parameter? car) parameters)))
        (values normal-parameters special-parameters))))

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

  (define tex-parameter-object
    "\\item[Parameter] \\texttt{@PARAMETER@}")

  (define tex-syntax
    "\\item[Syntax] \\texttt{@NAME-AND-FORMALS@ $\\to$ @TO@}")

  (define tex-parameters
    "\\item[Parameters]
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
            `((title . ,(car arguments)))))))))

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
      (write-template
       tex-source
       `((source .
                 ,(with-output-to-string
                    (lambda ()
                      (pp expr))))
         (name . ,name)))))

  (define (make-tex-procedure template name formals to)
    (substitute-template
     template
     `((name-and-formals . ,(texify (cons name formals)))
       (to . ,(string-join (map texify to) ", ")))))

  (define (make-tex-parameters parameters)
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

  (define (tex-procedure-to special-parameters)
    (alist-ref/default special-parameters '@to '("unspecified")))

  (define (tex-parse-procedure doc expr data name formals)
    (receive (normal-parameters special-parameters)
      (doc-normal-and-special-parameters doc)
      (let ((to (tex-procedure-to special-parameters)))
        (let ((procedure
               (make-tex-procedure tex-procedure name formals to))
              (parameters (make-tex-parameters normal-parameters)))
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
      (2 tex-subsubheading)))

  (define (tex-make-description descriptions)
    (substitute-template
     tex-item-description
     'description
     (string-join (map texify descriptions) "\n\n")))

  (define (tex-parse-scalar doc expr data name)
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
         scalar))))

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
      (let ((to (tex-procedure-to special-parameters)))
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
          (let ((parameters (make-tex-parameters normal-parameters)))
            (lambda ()
              (write-tex-block
               doc
               expr
               data
               name
               case-lambda
               parameters)))))))

  (define (formals parameters) (map car parameters))

  (define (tex-parse-syntax doc expr data name)
    (receive (normal-parameters special-parameters)
      (doc-normal-and-special-parameters doc)
      (let ((to (tex-procedure-to special-parameters)))
        (let ((syntax
               (make-tex-procedure
                tex-syntax
                name
                (formals normal-parameters)
                to))
              (parameters
               (make-tex-parameters normal-parameters)))
          (lambda ()
            (write-tex-block
             doc
             expr
             data
             name
             syntax
             parameters))))))

  (define (tex-parse-docexpr document docexpr)
    (parameterize ((parse-directive tex-parse-directive)
                   (parse-procedure tex-parse-procedure)
                   (parse-case-lambda tex-parse-case-lambda)
                   (parse-parameter tex-parse-parameter)
                   (parse-scalar tex-parse-scalar)
                   (parse-syntax tex-parse-syntax))
      (parse-docexpr document docexpr)))

  (define (tex-parse-docexprs document docexprs)
    (let ((parsed-docexprs (make-stack)))
      (stack-for-each
       docexprs
       (lambda (docexpr)
         (stack-push! parsed-docexprs
                      (tex-parse-docexpr document docexpr))))
      parsed-docexprs))

  (define (parse-file file)
    (parameterize ((docexprs (make-stack)))
      (with-input-from-file file
        (lambda ()
          (let read-next ((expression (read)))
            (if (not (eof-object? expression))
                (begin
                  (if (current-docexpr)
                      (docexpr-expr-set! (stack-peek (docexprs)) expression))
                  (current-docexpr #f)
                  (read-next (read)))))))
      (docexprs)))

  (define (tex-write-docexprs docexprs)
    (let* ((document (make-document (make-hash-table) (make-stack)))
           (parsed-docexprs (tex-parse-docexprs document (docexprs))))
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
      (display tex-footer))))
