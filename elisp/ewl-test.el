;; -*- lexical-binding: t; -*-
(require 'ewl)

;; message to stderr
(defun message (format-string &rest args)
  (princ (concat (apply #'format format-string args) "\n") #'external-debugging-output))

;; (toggle-debug-on-error)
;; (tool-bar-mode -1)
;; (menu-bar-mode -1)

;; (ewl-init)
;; (ewl-start-client "wayland-0")

(let ((process-environment
       (cons "WLR_X11_OUTPUTS=1"
             ;; DEBUG
             (cons "WAYLAND_DEBUG=1"
                   process-environment))))
  (ewl-init t))
