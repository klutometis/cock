(module cock
  ()
  (import chicken scheme)

  (set-read-syntax! #\@ (lambda (in) (read in) (values))))
