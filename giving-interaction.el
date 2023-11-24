;;; giving-interaction --- Forth interaction -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'comint)
(require 'project)
;; (require 'forth-mode)

(define-derived-mode giving/interaction-mode comint-mode "Forthsgiving"
  "Major mode for Forth interaction."
  :group 'giving)

(defun giving/get-interaction-buffer ()
  "Return the Forth interaction buffer for this project."
  (let* ((proj (project-current))
         (root (and proj (project-root proj)))
         (nm (if proj (format "*giving-interaction %s*" root) "*giving*")))
    (unless (get-buffer nm)
      (let ((buf (get-buffer-create nm)))
        (with-current-buffer buf
          (unless (comint-check-proc buf)
            (let ((process-environment (cons "TERM=dumb" process-environment)))
              (make-comint-in-buffer "giving-interaction" buf "gforth")))
          (giving/interaction-mode))))
    (get-buffer nm)))

(defun giving/interaction-send (str)
  "Send STR to Forth."
  (let ((buf (giving/get-interaction-buffer)))
    (comint-send-string
     (get-buffer-process buf)
     (format "%s\n" str))))

(provide 'giving-interaction)
;;; giving-interaction.el ends here
