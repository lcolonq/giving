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
     :name "restart"
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
     :name "char0"
     :code "
\tld de, 48
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "twohundredfifty"
     :code "
\tld de, 250
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "zero"
     :code "
\tld de, 0
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "one"
     :code "
\tld de, 1
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "two"
     :code "
\tld de, 2
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "three"
     :code "
\tld de, 3
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "four"
     :code "
\tld de, 4
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "five"
     :code "
\tld de, 5
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "six"
     :code "
\tld de, 6
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "seven"
     :code "
\tld de, 7
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "ten"
     :code "
\tld de, 10
\tGPush d,e
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
     :name "bitand"
     :code "
\tGPop b,c
\tGPop d,e
\tld a,d
\tand a,b
\tld d,a
\tld a,e
\tand a,c
\tld e,a
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
     :name "drop"
     :code "
\tGPop b,c
\tjp NEXT
")
    (giving/make-word
     :name "incb2"
     :body '("incb" "incb" "exit")
     )
    (giving/make-word
     :name "define"
     :comptime
     (lambda (st)
       (giving/advance-start-of-word st)
       (let ((word-name (giving/parse-next-word st)))
         (push (cons word-name nil) (giving/compiler-state-word-stack st)))))
    (giving/make-word
     :name "end"
     :comptime
     (lambda (st)
       (giving/define-current-word st)))
    (giving/make-word
     :name "includefile1"
     :comptime
     (lambda (st)
       (let ((path (giving/parse-next-word st)))
         (giving/add-extra-data st (format "
SECTION \"File Data 1\", ROMX, BANK[1]
FILE1DATA:
INCBIN \"%s\"" path)))))
    (giving/make-word
     :name "includefile2"
     :comptime
     (lambda (st)
       (let ((path (giving/parse-next-word st)))
         (giving/add-extra-data st (format "
SECTION \"File Data 2\", ROMX, BANK[2]
FILE2DATA:
INCBIN \"%s\"" path)))))
    (giving/make-word
     :name "file1"
     :code "
\tld hl,FILE1DATA
\tld e,l
\tld d,h
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "file2"
     :code "
\tld hl,FILE2DATA
\tld e,l
\tld d,h
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "variable"
     :comptime
     (lambda (st)
       (let ((name (giving/parse-next-word st)))
         (giving/add-extra-vars st (format "
var_%s: dw
" name))
       (giving/define-word
        st
        (giving/make-word
         :name name
         :code (format "
\tld hl,var_%s
\tld e,l
\tld d,h
\tGPush d,e
\tjp NEXT
" name))))))
    (giving/make-word
     :name "readbyte"
     :code "
\tGPop b,c
\tld l,c
\tld h,b
ld a, [hl]
ld c, a
ld b, 0
\tGPush b,c
\tjp NEXT
")
    (giving/make-word
     :name "writebyte"
     :code "
\tGPop b,c
\tGPop d,e
\tld l,c
\tld h,b
ld a, e
ld [hl], a
\tjp NEXT
")
    (giving/make-word
     :name "readaddr"
     :code "
\tGPop b,c
\tld l,c
\tld h,b
ld a, [hl+]
ld c, a
ld a, [hl]
ld b, a
\tGPush b,c
\tjp NEXT
")
    (giving/make-word
     :name "writeaddr"
     :code "
\tGPop b,c
\tGPop d,e
\tld l,c
\tld h,b
ld a, e
ld [hl+], a
ld a, d
ld [hl], d
\tjp NEXT
")
    (giving/make-word
     :name "mark"
     :code "
\tGLoad GivingIP,b,c
\tld l,c
\tld h,b
\tdec hl
\tdec hl
\tld c,l
\tld b,h
\tGPush b,c
\tjp NEXT
")
    (giving/make-word
     :name "goto"
     :code "
\tGPop b,c
\tGStore GivingIP,b,c
\tjp NEXT
")
    (giving/make-word
     :name "skip"
     :code "
\tGLoad GivingIP,b,c
\tGPop d,e
\tld l,c
\tld h,b
\tsla e
\tld d,0
\tadd hl,de
\tGStore GivingIP,h,l
\tjp NEXT
")
    (giving/make-word
     :name "iseven"
     :code "
\tGPop d,e
\tld a,e
\tand a,1
\tld e,a
\tld d,0
\tGNegate d,e
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "iszero"
     :code "
\tGPop d,e
\tld a,e
\tcp a,0
\tjp z, .then
\tld e,0
\tld d,0
\tjp .end
.then:
\tld e,$ff
\tld d,$ff
.end:
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "isnewline"
     :code "
\tGPop d,e
\tld a,e
\tcp a,10
\tjp z, .then
\tld e,0
\tld d,0
\tjp .end
.then:
\tld e,$ff
\tld d,$ff
.end:
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "isnumber"
     :code "
\tGPop d,e
\tld a,e
\tsub a,48
\tjp c,.false
\tld a,e
\tsub a,58
\tjp nc,.false
\tld e,$ff
\tld d,$ff
\tGPush d,e
\tjp NEXT
.false:
\tld e,0
\tld d,0
\tGPush d,e
\tjp NEXT
")
    (giving/make-word
     :name "debugbreak"
     :code "
\tGLoad var_total,d,e
__DEBUG:
\tjp NEXT
")
    )
   )
  )

(defun giving/backend-header (backend)
  "Return the header for an assembly file for BACKEND."
  (funcall (giving/backend-header-template backend) backend))

(defconst giving/backend-gb (giving/make-backend))

(provide 'giving-backend)
;;; giving-backend.el ends here
