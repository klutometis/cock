(define-syntax run-cock
  (ir-macro-transformer
   (lambda (expression rename inject)
     (let ((invocation (cdr expression)))
       `(handle-exceptions exn
          (warning (format "Documentation not generated: ~a"
                           ((condition-property-accessor 'exn 'message) exn)))
          (run (cock ,@invocation)))))))
