;;; giving-word --- Words and dictionaries -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)

(cl-defstruct
    (giving/word
     (:constructor giving/make-word))
  name
  (code nil) ;; either nil for ENTER or the actual assembly as a string
  (body nil) ;; list of names of body words
  (comptime nil) ;; compile-time semantics (a function of compiler state, return value ignored)
  )

(provide 'giving-word)
;;; giving-word.el ends here
