(module cock
  (prepend-@)
  (import chicken scheme)
  (use coerce debug)

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
