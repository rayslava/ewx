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

;; each frame needs a layout function that knows its global position and span

;; [1] NEXT: On frame creation ask where to put new output, link with frame and display there.
;;           put on existing output -> stack
;;                | floating
;;                | unmapped output -> map top | right | bottom | left

;; 2nd take

;; surface -> layout -> view

;; different kind of surfaces:
;;   1. emacs frame
;;   2. wayland buffer
;;   3. wayland widget 

;; detect 1 and 3 by pid -> surface dispatch mechanism

;; 1: triggered by new output -> make-frame & add layout function (frame parameter?)
;;                 surface -> link with frame somehow? & layout fullscreen

;; 2: default for new surface -> make-buffer & link with surface (buffer-local)

;; Setup ewb buffer to create/delete/update view on show/hide/update

;; Output size change -> relayout linked frame

;; (window-body-pixel-edges)
;; => (8 0 952 1150)

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

;;; Init
;; ewb-init with tmp names: ewb-1 -> ewb-2
(defun ewb-1 ()
  (let ((process-environment
         (cons "WLR_X11_OUTPUTS=2"
               ;; DEBUG
               (cons "WAYLAND_DEBUG=1"
                     process-environment))))
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
                             ;; TODO: Use pkg-config?
                             ;;   pkg-config --variable=pkgdatadir wayland-protocols
                             ("/gnu/store/4xinswliqgki9sxkffh1gg4fdymca0ph-wayland-protocols-1.26/share/wayland-protocols/unstable/xdg-output/xdg-output-unstable-v1.xml"
                              zxdg-output-manager-v1 zxdg-output-v1) ; = all interfaces
                             ("ewp.xml"
                              ewp-layout ewp-surface ewp-view))
                   (expand-file-name socket (xdg-runtime-dir))))
         (registry (ewc-object-add :objects objects
                                   :protocol 'wayland
                                   :interface 'wl-registry))
         xdg-output-manager
         (outputs (list 'outputs)))
    ;; Make a struct for above?
    
    ;; Add global listener
    (setf (ewc-listener registry 'global)
          (pcase-lambda (object (map name interface version))
            ;; DEBUG
            (message "Global %s %s %s" name interface version)
            (message "outputs %s" outputs)
            (pcase interface
              ("zxdg_output_manager_v1" (setq xdg-output-manager
                                              (ewb-output-xdg-manager registry name version)))
              ("ewp_layout" (ewb-init-layout registry name version outputs))
              ("wl_output" (ewb-output-new registry xdg-output-manager outputs name version)))))

    (ewc-request (ewc-object-get 1 objects) 'get-registry `((registry . ,(ewc-object-id registry))))))

(let ((o))
  (setf (alist-get 1 o) nil)
  (push 33 (setf (alist-get 2 o) (list 11)))
  o)
;; => ((2 11) (1))

;; => nil


;;; Outputs
;; wlroots has a output layout containing all outputs in one coordinate system.
;; wlr_output_layout_add(x,y) adds the output at x and y
;; wlr_output_layout_add_auto add the output to the right AND is used in ews.c

;; wayland.xml says the following for wl_output:
;;   Instead of using x and y, clients should use
;;   xdg_output.logical_position. Instead of using make and model,
;;   clients should use name and description.

;; X11 outputs, which I use for testing, have x=0, y=0 regardless of layout
;;   -> do the offset calculation here for now
;;      and use a nice struct

;; TODO: Use xdg_output output instead of wl_output!

(cl-defstruct (ewb-output (:constructor ewb-output-make)
                          (:copier nil))
  (x 0 :type natnum :documentation "Top left x coordinate in output layout")
  (y 0 :type natnum :documentation "Top left y coordinate in output layout")
  (width 0 :type natnum :documentation "Output width")
  (height 0 :type natnum :documentation "Output height")
  (name nil :type string :documentation "Output name")
  (description nil :type string :documentation "Output description"))
;; Add frame slot?

;; TODO: Abstract; this is simple listener free version.
(defun ewb-output-xdg-manager (registry name version)
  (cl-assert (eql version 3))
  (let ((xdg-output-manager (ewc-object-add :objects (ewc-object-objects registry)
                                            :protocol 'xdg-output-unstable-v1
                                            :interface 'zxdg-output-manager-v1)))
    (ewc-request registry 'bind `((name . ,name)
                                  (interface-len . ,(1+ (length "zxdg_output_manager_v1")))
                                  (interface . "zxdg_output_manager_v1")
                                  (version . ,version)
                                  (id . ,(ewc-object-id xdg-output-manager))))
    xdg-output-manager))

;; (defun ewb-output-update (outputs id update)
;;   (pcase-let ((`(,output . ,dependends) (assoc id outputs #'eql)))
;;     (while update
;;       (pcase (pop update)
;;         (`(name . ,name)
;;          (setf (ewb-output-name output) name))
;;         (`(description . ,description)
;;          (setf (ewb-output-description output) description))))))

;; Idea: every output has a function/closure that takes the update and applies it
;;       ie changes output data & resizes frame
;;       who else needs access to output data? (something like list-outputs?)
;;       how to provide it?
(defun ewb-output-update (output &rest updates)
  )

(let ((update))
  (defun ewb-output-listener (event)
    (pcase event
      ((or 'logical-position 'logical-size 'name 'description)
       (lambda (_object values)
         (push values update)))
      ('done
       (lambda (object _values)
         (cl-loop for (field . value) in update
                  do (ewb-output-update (ewc-object-data object) field value))
         (setq update nil)
         (message "Updated outputs: %s" object)))))) ; DEBUG

;; TODO: Should set listeners only once. Not for each new output.
(defun ewb-output-new (registry xdg-output-manager outputs name version)
  (cl-assert (eql version 4))
  (let ((output (ewc-object-add :objects (ewc-object-objects registry)
                                :protocol 'wayland
                                :interface 'wl-output
                                :data (ewb-output-make))))
    (push (ewc-object-id output) (cdr outputs))
    (message "outputs %s" outputs)      ; DEBUG

    (setf (ewc-listener output 'done) (ewb-output-listener 'done))
    
    (ewc-request registry 'bind `((name . ,name)
                                  (interface-len . ,(1+ (length "wl_output")))
                                  (interface . "wl_output")
                                  (version . 4)
                                  (id . ,(ewc-object-id output))))

    (let ((xdg-output (ewc-object-add :objects (ewc-object-objects registry)
                                      :protocol 'xdg-output-unstable-v1
                                      :interface 'zxdg-output-v1)))

      (dolist (event '(logical-position logical-size name description))
        (setf (ewc-listener xdg-output event) (ewb-output-listener event)))
      
      (ewc-request xdg-output-manager 'get-xdg-output `((id . ,(ewc-object-id xdg-output))
                                                        (output . ,(ewc-object-id output)))))))

;;; Layout
(defun ewb-init-layout (registry name version outputs)
  (cl-assert (eql version 1))
  (let ((layout (ewc-object-add :objects (ewc-object-objects registry)
                                :protocol 'emacs-wayland-protocol
                                :interface 'ewp-layout)))
    (setf (ewc-listener layout 'new-surface)
          #'ewb-new-surface)
    (ewc-request registry 'bind `((name . ,name)
                                  (interface-len . ,(1+ (length "ewp_layout")))
                                  (interface . "ewp_layout")
                                  (version . 1)
                                  (id . ,(ewc-object-id layout))))))

(defun ewb-new-surface (object args)
  (message "New surface: %s" args)
  (pcase-let (((map id app_id title pid) args))
    (ewc-object-add :objects objects
                    :protocol 'emacs-wayland-protocol
                    :interface 'ewp-surface
                    :id id
                    ;; :data app_id title pid ?
                    )
    ;; handle update-title and destroy events

    ;; add buffer and link to surface
    ))

;; NEXT:
;; - Rig up frame
;; - Rig up display of surface (= ewp-view)

;; TODO:
;; - Abstract common pattern: ewc-object-add -> objects & ewc-request with object-id in args

(provide 'ewb)
;;; ewb.el ends here
