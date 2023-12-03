;;; giving-backend --- Architecture backends -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'cl-lib)

(defconst giving/base-path (f-parent (or load-file-name (buffer-file-name))))

(defun giving/slurp (path)
  "Read PATH and return a string."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(cl-defstruct
    (giving/backend
     (:constructor giving/make-backend))
  (name "gb")
  (cell-size 2) ;; cell size in bytes
  (assemble-command
   (lambda (in out)
     (list
      "rgbasm"
      "-I" (f-join giving/base-path "data" "gb")
      "-L"
      "-o" out
      in)))
  (link-command (lambda (in out) (list "rgblink" "-n" "symbols.txt" "-o" out in)))
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
     (giving/slurp (f-join giving/base-path "data" "gb" "header.s"))))
  (code
   "
NEXT:
;; load IP into de
\tGLoad GivingIP,d,e
;; load entry address for word at IP into de
\tGDeref d,e
;; hl now contains next IP value, update GivingIP
\tGStore GivingIP,h,l
;; update GivingW with value of entry address for word
\tGStore GivingW,d,e
;; load address of code for entry into de
\tGDeref d,e
;; jump to code
\tld l, e
\tld h, d
\tjp hl

ENTER:
;; load GivingIP into hl and push it
\tGLoad GivingIP,h,l
\tpush hl
;; load GivingW into hl
\tGLoad GivingW,h,l
;; increment by 2
\tinc hl
\tinc hl
;; set GivingIP to the incremented GivingW
\tGStore GivingIP,h,l
\tjp NEXT

EXIT:
;; pop the previous IP from stack and restore it
\tpop hl
\tGStore GivingIP,h,l
\tjp NEXT
exit:
\tdw EXIT
")
  (prelude
   (list
    (giving/make-word
     :name "quit"
     :code "
\tjp Main
")
    (giving/make-word
     :name "incb"
     :code "
\tinc b
\tjp NEXT
")
    (giving/make-word
     :name "cafe"
     :code "
\tld de, $cafe
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "beef"
     :code "
\tld de, $beef
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "plus"
     :code "
\tGPop b,c
\tGPop d,e
\tld l,e
\tld h,d
\tadd hl,bc
\tld c,l
\tld b,h
\tGPush b,c
\tjp NEXT
")
    (giving/make-word
     :name "minus"
     :code "
\tGPop b,c
\tGNegate b,c
\tGPop d,e
\tld l,e
\tld h,d
\tadd hl,bc
\tld c,l
\tld b,h
\tGPush b,c
\tjp NEXT
")
    (giving/make-word
     :name "dup"
     :code "
\tGPop b,c
\tGPush b,c
\tGPush b,c
\tjp NEXT
")
    (giving/make-word
     :name "swp"
     :code "
\tGPop b,c
\tGPop d,e
\tGPush b,c
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "debug"
     :code "
\tGPop b,c
\tjp NEXT
")
    (giving/make-word
     :name "incb2"
     :body '("incb" "incb" "exit")
     )
    (giving/make-word
     :name ":"
     :comptime
     (lambda (st)
       (giving/advance-start-of-word st)
       (let ((start (giving/compiler-state-index st)))
         (giving/advance-end-of-word st)
         (let* ((end (giving/compiler-state-index st))
                (word-name (substring (giving/compiler-state-source st) start end)))
           (push (cons word-name nil) (giving/compiler-state-word-stack st))))))
    (giving/make-word
     :name ";"
     :comptime
     (lambda (st)
       (giving/define-current-word st)))
    )
   )
  )

(defun giving/backend-header (backend)
  "Return the header for an assembly file for BACKEND."
  (funcall (giving/backend-header-template backend) backend))

(defconst giving/backend-gb (giving/make-backend))

(provide 'giving-backend)
;;; giving-backend.el ends here
