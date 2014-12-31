(module cock
  (at run-cock)
  (import chicken scheme)

  (define-syntax run-cock
    (ir-macro-transformer
     (lambda (expression rename inject)
       `(handle-exceptions exn
          (warning (format "Documentation not generated: ~a

This may be because cock-utils is not installed. Cock-utils is an
optional egg for generating documentation and installation will
succeed without it."
                           ((condition-property-accessor 'exn 'message) exn)))
          (run (cock ,@(cdr expression)))))))

  ;; This is a hack to provide unescaped @s.
  (define at '@)

  (define (prepend-@ symbol)
    (string->symbol
     (string-append "@" (symbol->string symbol))))

  (set-read-syntax! #\@
    (lambda (in)
      (let ((expression (read in)))
        (cond ((pair? expression)
               (values))
              ((symbol? expression)
               (prepend-@ expression))
              (else expression))))))
