#!/usr/bin/env chicken-scheme

(use alist-lib
     args
     cock-parse
     debug
     files
     miscmacros
     posix
     shell
     usage)

(define options
  (make-parameter
   (list (args:make-option (h ? help) #:none "Help" (set! arg #t))
         (args:make-option (l latex) #:none "Output to LaTeX" (set! arg #t))
         (args:make-option (o output) (required: "FILE") "Output to FILE")
         (args:make-option (p pdf) #:none "Output to PDF [Default]" (set! arg #t)))))

(define usage
  (make-usage
   (lambda (program)
     (format #t "Usage: ~a [OPTIONS]... FILE...~%" program)
     (print (args:usage (options))))))

(define (parse-and-write files)
  (tex-write-docexprs (apply parse-files files)))

(define (option options option)
  (alist-ref/default options option #f))

(define (with-working-directory directory thunk)
  (let ((original-directory (current-directory)))
    (dynamic-wind (lambda () (current-directory directory))
        thunk
        (lambda () (current-directory original-directory)))))

(receive (options files)
  (args:parse (command-line-arguments) (options))
  ;; Let's abstract these.
  (let ((help (option options 'help))
        (latex (option options 'latex))
        (output (option options 'output)))
    (cond (help (usage))
          ((null? files) (usage 1))
          (latex
           (if output
               (with-output-to-file output
                 (lambda () (parse-and-write files)))
               (parse-and-write files)))
          (else
           (let ((output-directory (create-temporary-directory))
                 (current-directory (current-directory)))
             (with-working-directory output-directory
               (lambda ()
                 (dotimes (pass 3)
                   (with-output-to-pipe
                    (format "xelatex -shell-escape -output-directory=~a"
                            output-directory)
                    (lambda ()
                      (parse-and-write
                       (map (lambda (file)
                              (if (absolute-pathname? file)
                                  file
                                  (make-pathname current-directory file)))
                            files)))))))
             (when output
               (file-copy (make-pathname output-directory "texput.pdf")
                          output
                          #t)))))))
