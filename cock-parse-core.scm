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
  @("Composite documentation and adherent expression"
    (doc "Documentation for the expression")
    (expr "Expression surrounding the documentation"))
  doc
  expr)

(define parse-directive (make-parameter void))

(define parse-procedure (make-parameter void))

(define parse-scalar (make-parameter void))

(define parse-parameter (make-parameter void))

(define parse-case-lambda (make-parameter void))

(define parse-syntax (make-parameter void))

(define parse-read (make-parameter void))

(define parse-record (make-parameter void))

(define parse-module (make-parameter void))

;;; Somehow, we have to process these preamble-directives before we
;;; spit the document out; could it be that we have to keep the thing
;;; in memory before we spit it out?
;;;
;;; The document has some header fields and a list of docexprs: thus,
;;; we can process the docexprs in order, pushing to the section
;;; stack; &c.
;;;
;;; Should we say, more formally, that directives are things which
;;; work on the document; and have first-class support for things like
;;; sections?
;;;
;;; Sections, &c. could work, I suppose, by pushing something unto the
;;; docexprs stack.
;;;
;;; It's a shame, though, that the document-fields are fixed; and that
;;; directives don't have the ability to put arbitrary data in there.
;;; Why not add a hash-table called data?
;;;
;;; The idea is that the renderers check for some kind of field in the
;;; hash-table, supplying a reasonable default.
;;;
;;; It's a shame, though, that we have to special case so-called
;;; directives; can every parsed docexpr work on the document?
;;;
;;; Non-directive docexprs would have to push themselves on the
;;; docexpr-stack, though.
;;;
;;; Why not push every docexpr on the stack and convert the directives
;;; into no-ops? Bingo.
;;;
;;; docexprs are lambdas: at construction time, they take a document they
;;; can modify. At invocation time, they write something.
;;;
;;; If we were to support more than latex, though, how would they know
;;; to dispatch? Do we need an e.g. write-docexpr-as-{html,latex},
;;; such that we need to maintain the types?
;;;
;;; If I go with the dispatch-on-type, though, I have to come up with
;;; types for e.g. headings and subheading; which is a pain in the
;;; ass. Oh, for pure lambdas!
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
      ((or ('define (procedure . formals) . body)
           ('define procedure ('lambda formals . body)))
       ((parse-procedure) doc expr data procedure formals))
      (('define procedure ('case-lambda (formals . body) ...))
       ((parse-case-lambda) doc expr data procedure formals))
      (('define parameter ('make-parameter init . converter))
       ((parse-parameter) doc expr data parameter init))
      (('define scalar . body)
       ((parse-scalar) doc expr data scalar))
      (('define-syntax name . _)
       ((parse-syntax) doc expr data name))
      ((or ('set-read-syntax! char-or-symbol proc)
           ('set-sharp-read-syntax! char-or-symbol proc)
           ('set-parameterized-read-syntax! char-or-symbol proc))
       ((parse-read) doc expr data char-or-symbol))
      ((or ('define-record-type type . fields)
           ('define-record type . fields)
           ('defstruct type . fields))
       ((parse-record) doc expr data type))
      (('module module exports . body)
       ((parse-module) doc expr data module exports)) 
      ;; Here's where we might make the thing extensible; or maybe
      ;; initially, to give people the opportunity to override the
      ;; above?
      ;;
      ;; Don't know what the fuck this is: let's treat it like a
      ;; directive, for the time being.
      (_ ((parse-directive) doc null-expression data document)))))

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


;;; Shouldn't we let the caller pass in its own docexprs?
(define (parse-files . files)
  @("Parse files into docexprs."
    (files "Cock-documented files to be parsed")
    (@to "Resultant docexprs"))
  (parameterize ((docexprs (make-stack)))
    (for-each
        (lambda (file)
          (with-input-from-file file
            (lambda ()
              (let read-next ((expression (read)))
                (if (not (eof-object? expression))
                    (begin
                      (if (current-docexpr)
                          (docexpr-expr-set! (stack-peek (docexprs)) expression))
                      (current-docexpr #f)
                      (read-next (read))))))))
      files)
    (docexprs)))
