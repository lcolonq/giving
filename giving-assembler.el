;;; giving-assembler --- Assembler -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)

(defcustom giving/assembly-buffer "*giving-assembly*"
  "Name of buffer used to display assembly code."
  :type '(string)
  :group 'giving)

(defcustom giving/assembler-output-buffer "*giving-assembler*"
  "Name of buffer used to display assembler output."
  :type '(string)
  :group 'giving)

(defun giving/call-assembler-process (argv)
  "Invoke ARGV synchronously and direct output to `giving/assembler-output-buffer'."
  (with-current-buffer (get-buffer-create giving/assembler-output-buffer)
    (erase-buffer)
    (let ((status
           (apply
            #'call-process
            (-concat
             (list (car argv) nil t nil)
             (cdr argv)))))
      (unless (= status 0)
        (error
         (format "Giving assembler process failed: %s" (buffer-string)))))))

(defun giving/assemble (path backend words)
  "Given a BACKEND and a list of WORDS, produce an executable at PATH."
  (let* ((extra-vars (giving/word-code (pop words)))
         (extra-data (giving/word-code (pop words)))
         (asm-path (make-temp-file "giving-assemble"))
         (obj-path (make-temp-file "giving-link"))
         (assemble (funcall (giving/backend-assemble-command backend) asm-path obj-path))
         (link (funcall (giving/backend-link-command backend) obj-path path))
         (post (funcall (giving/backend-post-command backend) path))
         (extra-code
          (s-join
           "\n"
           (--map
            (format "%s:\n%s" (s-upcase (giving/word-name it)) (giving/word-code it))
            (-filter #'giving/word-code words))))
         (words-code
          (s-join
           "\n"
           (--map
            (funcall
             (giving/backend-define-word backend)
             (giving/word-name it)
             (if (giving/word-code it)
                 (s-upcase (giving/word-name it))
               "ENTER")
             (giving/word-body it))
            words)))
         (all-code
          (s-join
           "\n"
           (list
            (giving/backend-header backend)
            "SECTION \"User RAM\", WRAM0"
            extra-vars
            "SECTION \"User ROM\", ROM0"
            (giving/backend-code backend)
            extra-code
            words-code
            extra-data
            )))
         )
    (with-current-buffer (get-buffer-create giving/assembly-buffer)
      (erase-buffer)
      (asm-mode)
      (insert all-code))
    (write-region all-code nil asm-path)
    (giving/call-assembler-process assemble)
    (giving/call-assembler-process link)
    (giving/call-assembler-process post)
    ))

(giving/assemble
 "test.gb"
 giving/backend-gb
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
   :name "main"
   :body '("beef" "cafe" "swp" "minus" "debug" "quit")
   )
  )
 )

(provide 'giving-assembler)
;;; giving-assembler.el ends here
