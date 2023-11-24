;;; giving-backend --- Architecture backends -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'cl-lib)

(defconst giving/base-path (f-parent (or load-file-name (buffer-file-name))))

(cl-defstruct
    (giving/backend
     (:constructor giving/make-backend))
  (name "gb")
  (cell-size 2) ;; cell size in bytes
  (assemble-command (lambda (in out) (list "rgbasm" "-L" "-o" out in)))
  (link-command (lambda (in out) (list "rgblink" "-o" out in)))
  (post-command (lambda (in) (list "rgbfix" "-v" "-p" "0xFF" in)))
  (define-word
   (lambda (name code-label word-labels)
     (format
      "%s:\n\tdw %s\n%s"
      (s-downcase name)
      code-label
      (if word-labels
          (format "\tdw %s\n" (s-join "," word-labels))
        ""))))
  (header-template
   (lambda (backend)
     (format
      "INCLUDE \"%s\"
SECTION \"Header\", ROM0[$100]
\tjp EntryPoint
\tds $150 - @, 0
EntryPoint:
\tld a, 0
\tld [rNR52], a
Main:
\tinc de
\tjp Main
SECTION \"Giving Forth\", ROM0
"
      (f-join (giving/backend-data-dir backend) "hardware.inc"))))
  (code
   "NEXT:
\tld e,[hl]
\tinc hl
\tld d,[hl]
\tinc hl
\tpush hl
\tld h,d
\tld l,e
\tjp [hl]
ENTER:
\tinc hl
\tinc hl
\tjp NEXT
EXIT:
\tpop hl
\tjp NEXT
"))

(defun giving/backend-data-dir (backend)
  "Return the absolute path to the data directory for BACKEND."
  (f-join giving/base-path "data" (giving/backend-name backend)))

(defun giving/backend-header (backend)
  "Return the header for an assembly file for BACKEND."
  (funcall (giving/backend-header-template backend) backend))

(defconst giving/backend-gb (giving/make-backend))

(provide 'giving-backend)
;;; giving-backend.el ends here
