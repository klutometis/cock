#!/usr/bin/env chicken-scheme

(use alist-lib args cock-parse debug usage)

(define options
  (list (args:make-option (l latex) #:none "Output to LaTeX" (set! arg #t))
        (args:make-option (h ? help) #:none "Help" (set! arg #t))))

(define usage
  (make-usage
   (lambda (program)
     (format #t "Usage: ~a [OPTIONS]... FILE...~%" program)
     (print (args:usage options)))))

(receive (options files)
  (args:parse (command-line-arguments) options)
  (cond ((alist-ref/default options 'help #f) (usage))
        ((null? files) (usage 1))
        (else (tex-write-docexprs (apply parse-files files)))))
