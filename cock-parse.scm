@(title "Cock: Inline Docs for Chicken Scheme")
@(author "Peter Danenberg")
@(email "pcd@roxygen.org")
@(heading "Cock-parse")

(module cock-parse
  @("The cock-parse module is responsible for the heavy lifting:
creating docexprs (see below) from documented sources code; the
drivers then write docexprs as e.g. LaTeX.")
  (parse-files
   tex-write-docexprs
   wiki-write-docexprs)
  (import chicken
          data-structures
          extras
          ports
          scheme
          srfi-1
          stack)
  (use alist-lib
       cock
       debug
       irregex
       lolevel
       matchable
       regex
       srfi-13
       srfi-69
       stack)

  (import-for-syntax matchable)

  (include "cock-parse-core.scm")
  (include "cock-parse-latex.scm")
  (include "cock-parse-wiki.scm"))
