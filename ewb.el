;; -*- lexical-binding: t; -*-

;;; Commentary:
;; emacs wayland buffer

;; outputs
;; wayland frames
;; wayland buffers

;; outputs <-> frames  (Linked with algo: If new frame link it to next output without frame, else stack [1])
;; -> frame knows its position inside output(-layout?) via link
;;    with pgtk window-body-pixel-edges is relative to emacs frame NOT output (this needs to be added; confirm with Po Lu?)
;; buffer issues layout command via frame who offsets window-body-pixel-edges to output-layout coordinates

;; non layed out window is hidden

;; [1] NEXT: On frame creation ask where to put new output, link with frame and display there.
;;           put on existing output -> stack
;;                | floating
;;                | unmapped output -> map top | right | bottom | left

;;; Research:

;; (make-frame)
;; Before creating the frame (via ‘frame-creation-function’), this
;; function runs the hook ‘before-make-frame-hook’.  After creating
;; the frame, it runs the hook ‘after-make-frame-functions’ with one
;; argument, the newly created frame.

;; If a display parameter is supplied and a window-system is not,
;; guess the window-system from the display.

;; (frame-list)
;; => (#<frame ewb.el - GNU Emacs at muh 0xf869b8>)

;;; Code:
(require 'ewc)

;; ewb-init with tmp names: ewb-1 -> ewb-2
(defun ewb-1 ()
  (let ((process-environment
         ;; DEBUG
         (cons "WAYLAND_DEBUG=1"
               process-environment)))
    (make-process
     :name "emacs-wayland-server"
     :buffer "*emacs-wayland-server*"
     :command (list (expand-file-name "./ews"))
     :filter (lambda (proc str)
               (with-current-buffer (process-buffer proc)
                 (goto-char (point-max))
                 (save-excursion (insert str))
                 (when (re-search-forward (rx "WAYLAND_DISPLAY=" (group (+ (not control))))
                                          nil t)
                   (setf (process-filter proc) #'internal-default-process-filter)
                   (ewb-2 (match-string 1))))))))

;; set WAYLAND_DISPLAY inside emacs instead of arg
(defun ewb-2 (socket)
  (let* ((objects (ewc-connect
                   (ewc-read ("~/s/wayland/ref/wayland/protocol/wayland.xml"
                              wl-display wl-registry wl-output)
                             ("ewp.xml"
                              ewp-layout ewp-surface ewp-view))
                   (expand-file-name socket (xdg-runtime-dir))))
         (registry (ewc-object-add :objects objects
                                   :protocol 'wayland
                                   :interface 'wl-registry))
         outputs)
    ;; Add global listener
    (setf (ewc-listener registry 'global)
          (pcase-lambda (object (map name interface version))
            (pcase interface
              ("wl_output" (ewb-new-output objects registry outputs name version))
              ("ewp_layout" (ewb-init-layout objects registry name version)))))
    (ewc-request objects 1 'get-registry `((registry . ,(ewc-object-id registry))))))

(let ((update))
  (defun ewb-output-listener (outputs id event)
    (pcase event
      ((or 'geometry 'mode 'scale 'name 'description)
       (lambda (_object values)
         (push values update)))
      ('done
       (lambda (_object _values)
         (setf (alist-get id outputs)
               (apply #'map-merge 'alist
                      (alist-get id outputs)
                      (nreverse update)))
         (setq update nil)
         (message "Updated outputs: %s" outputs)))))) ; DEBUG

(defun ewb-new-output (objects registry outputs name version)
  (cl-assert (eql version 4))
  (let ((output (ewc-object-add :objects objects
                                :protocol 'wayland
                                :interface 'wl-output))
        (id (1+ (length outputs))))
    (dolist (event '(geometry mode scale name description done))
      (setf (ewc-listener output event) (ewb-output-listener outputs id event)))

    (ewc-request objects registry 'bind `((name . ,name)
                                          (interface-len . ,(1+ (length "wl_output")))
                                          (interface . "wl_output")
                                          (version . 4)
                                          (id . ,(ewc-object-id output))))))
;; use mode for resolution

(defun ewb-init-layout (objects registry name version)
  (cl-assert (eql version 1))
  (let ((layout (ewc-object-add :objects objects
                                :protocol 'emacs-wayland-protocol
                                :interface 'ewp-layout)))
    (setf (ewc-listener layout 'new-surface)
          #'ewb-new-surface)
    (ewc-request objects registry 'bind `((name . ,name)
                                          (interface-len . ,(1+ (length "ewp_layout")))
                                          (interface . "ewp_layout")
                                          (version . 1)
                                          (id . ,(ewc-object-id layout))))))

(defun ewb-new-surface (object args)
  (message "New surface: %s" args)
  (pcase-let (((map id app_id title pid) args))
    id))

;; TODO:
;; - Abstract common pattern: ewc-object-add -> objects & ewc-request with object-id in args

(provide 'ewb)
;;; ewb.el ends here
