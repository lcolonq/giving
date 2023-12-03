;;; giving-compiler --- Compiler -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)

(cl-defstruct
    (giving/compiler-state
     (:constructor giving/make-compiler-state))
  (stack nil)
  (dict nil)
  (word-stack nil)
  (source "")
  (index 0)
  )

(defun giving/initial-compiler-state (backend source)
  "Build the starting compiler state from BACKEND and SOURCE."
  (giving/make-compiler-state
   :stack nil
   :dict (giving/backend-prelude backend)
   :word-stack (list (cons "main" nil))
   :source source
   :index 0))

(defun giving/define-current-word (st)
  "Pop an in-progress word from the word stack of ST and add it to the dictionary."
  (let ((w (pop (giving/compiler-state-word-stack st))))
    (push
     (giving/make-word :name (car w) :body (reverse (cdr w)))
     (giving/compiler-state-dict st))))

(defun giving/lookup-word (st nm)
  "Lookup NM in the dictionary for ST."
  (let ((dict (giving/compiler-state-dict st)))
    (--find (s-equals? nm (giving/word-name it)) dict)))

(defun giving/run-word (st word)
  "Run the compiler-time semantics for WORD on ST."
  (let ((f (giving/word-comptime word)))
    (if f
        (funcall f st)
      (push (giving/word-name word)
            (cdr (car (giving/compiler-state-word-stack st)))))))

(defun giving/is-space (c)
  "Return non-nil if C is whitespace."
  (-contains? '(?\n ?\t ? ) c))

(defun giving/advance-end-of-word (st)
  "Advance ST to the first whitespace character after the current word."
  (while
      (not
       (or
        (>= (giving/compiler-state-index st) (length (giving/compiler-state-source st)))
        (giving/is-space
         (seq-elt
          (giving/compiler-state-source st)
          (giving/compiler-state-index st)))))
    (cl-incf (giving/compiler-state-index st))))

(defun giving/advance-start-of-word (st)
  "Advance ST to the next non-whitespace character."
  (while
      (and
        (< (giving/compiler-state-index st) (length (giving/compiler-state-source st)))
        (giving/is-space
         (seq-elt
          (giving/compiler-state-source st)
          (giving/compiler-state-index st))))
    (cl-incf (giving/compiler-state-index st))))

(defun giving/compile (backend source)
  "Compile SOURCE to a Forth dictionary (a list of `giving/word's).
BACKEND is the architecture backend."
  (let ((st (giving/initial-compiler-state backend source)))
    (while (< (giving/compiler-state-index st) (length (giving/compiler-state-source st)))
      (giving/advance-start-of-word st)
      (let ((start (giving/compiler-state-index st)))
        (giving/advance-end-of-word st)
        (let* ((end (giving/compiler-state-index st))
               (word-name (substring (giving/compiler-state-source st) start end)))
          (unless (string-empty-p word-name)
            (giving/run-word st (giving/lookup-word st word-name)))
        )))
    (giving/define-current-word st)
    (giving/compiler-state-dict st)))

(provide 'giving-compiler)
;;; giving-compiler.el ends here
