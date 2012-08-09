(module cock-parse
  @("The cock-parse module is responsible for the heavy lifting:
creating docexprs (see below) from documented sources code; the
drivers then write docexprs as e.g. LaTeX.")
  (parse-files
   tex-write-docexprs)
  (import alist-lib
          chicken
          data-structures
          extras
          ports
          scheme
          srfi-1
          stack)
  (use alist-lib
       cock
       debug
       define-record-and-printer
       lolevel
       matchable
       regex
       srfi-13
       srfi-69
       stack)

  (import-for-syntax matchable))
