;;; giving-compiler --- Compiler -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)

(defcustom giving/assembly-buffer "*giving-assembly*"
  "Name of buffer used to display assembly code."
  :type '(string)
  :group 'giving)

(defcustom giving/compiler-output-buffer "*giving-compiler*"
  "Name of buffer used to display compiler output."
  :type '(string)
  :group 'giving)

(cl-defstruct
    (giving/word
     (:constructor giving/make-word))
  name
  (code nil) ;; either nil for ENTER or the actual assembly as a string
  (body nil) ;; list of names of body words
  )

(defun giving/call-compiler-process (argv)
  "Invoke ARGV synchronously and direct output to `giving/compiler-output-buffer'."
  (with-current-buffer (get-buffer-create giving/compiler-output-buffer)
    (erase-buffer)
    (let ((status
           (apply
            #'call-process
            (-concat
             (list (car argv) nil t nil)
             (cdr argv)))))
      (unless (= status 0)
        (error
         (format "Giving compiler process failed: %s" (buffer-string)))))))

(defun giving/assemble (path backend words)
  "Given a BACKEND and a list of WORDS, produce an executable at PATH."
  (let* ((asm-path (make-temp-file "giving-assemble"))
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
               "ENTER"))
            words)))
         (all-code
          (s-join
           "\n"
           (list
            (giving/backend-header backend)
            (giving/backend-code backend)
            extra-code
            words-code
            )))
         )
    (with-current-buffer (get-buffer-create giving/assembly-buffer)
      (erase-buffer)
      (asm-mode)
      (insert all-code))
    (write-region all-code nil asm-path)
    (giving/call-compiler-process assemble)
    (giving/call-compiler-process link)
    (giving/call-compiler-process post)
    ))

(giving/assemble
 "test.gb"
 giving/backend-gb
 nil
 )

(provide 'giving-compiler)
;;; giving-compiler.el ends here