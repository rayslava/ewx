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

;; surface -> layout

;; different kind of surfaces:
;;   1. emacs frame
;;   2. wayland buffer
;;   3. wayland widget 

;; detect 1 and 3 by pid -> surface dispatch mechanism

;; 1: triggered by new output -> make-frame & add layout function (frame parameter?)
;;                 surface -> link with frame somehow? & layout fullscreen

;; 2: default for new surface -> make-buffer & link with surface (buffer-local)

;; Setup ewb buffer to create/delete/update layout on show/hide/update

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
(require 'xdg)

;;; Helper
(defun add-onetime-hook (hook function)
  (letrec ((wrapper (lambda (&rest args)
                      (let ((res (apply function args)))
                        (when res
                          (remove-hook hook wrapper))))))
    (add-hook hook wrapper)))

;;; Output & frame
;; a global data for all outputs:
;;   - xdg-output-manager
;;   - list of outputs

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

;; Idea: every output has a function/closure that takes the update and applies it
;;       ie changes output data & resizes frame
;;       who else needs access to output data? (something like list-outputs?)
;;       how to provide it?

(cl-defstruct (ewb-outputs (:constructor ewb-outputs-make)
                           (:copier nil))
  (xdg-output-manager nil :type ewc-object)
  (list nil :type list))             ; list of ewb-output TODO: Just have a list & keep xdg-output-manager seperate?

(cl-defstruct (ewb-output (:constructor ewb-output-make)
                          (:copier nil))
  (x nil :type natnum :documentation "Top left x coordinate in output layout")
  (y nil :type natnum :documentation "Top left y coordinate in output layout")
  (width nil :type natnum :documentation "Output width")
  (height nil :type natnum :documentation "Output height")
  (name nil :type string :documentation "Output name")
  (description nil :type string :documentation "Output description")
  (frame nil :type frame :documentation "Linked frame")
  (surface nil :type ewc-object :documentation "Surface of linked frame"))

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

(let ((update))
  (defun ewb-output-listener (event)
    (pcase event
      ((or 'logical-position 'logical-size 'name 'description)
       (lambda (_object values)
         (push values update)))
      ('done
       (lambda (object _values)
         (let ((update (apply #'nconc update)))
           (message "Received update: %s" update) ; DEBUG
           (ewb-output-update (ewc-object-data object) update))
         (setq update nil))))))

;; Split in -init and -new to set listeners only once and not for each new output.
(defun ewb-output-init (objects)
  (setf (ewc-listener-global objects 'wayland 'wl-output 'done)
        (ewb-output-listener 'done))
  
  (dolist (event '(logical-position logical-size name description))
    (setf (ewc-listener-global objects 'xdg-output-unstable-v1 'zxdg-output-v1 event)
          (ewb-output-listener event))))

(defun ewb-output-init-surface (output)
  (lambda (surface _app-id _title pid)
    (message "Init with pid %s=%s?" pid (emacs-pid))            ; DEBUG
    (when (eql pid (emacs-pid))
      (message "Init with pid %s!" pid) ; DEBUG
      (setf (ewb-output-surface output) surface)
      ;; frame-parameter is used by ewb-buffer-focus
      ;; TODO: Use frame-parameters for output handling?
      (setf (frame-parameter (ewb-output-frame output) 'ewb-surface) surface)
      ;; Try to layout frame. Succeeds when x y width height are already set.
      (ewb-output-layout-frame output)
      t)))

(defun ewb-output-new (registry outputs name version)
  (cl-assert (eql version 4))

  (let ((output (ewb-output-make)))

    (add-onetime-hook 'ewb-surface-functions (ewb-output-init-surface output))

    (setf (ewb-output-frame output)
          ;;             TODO: Pass env var/ integrate
          (make-frame '((display . "wayland-0"))))

    (message "Made a frame")            ; DEBUG
    (message "terminals %s" (terminal-list))            ; DEBUG
    
    (let ((wl-output (ewc-object-add :objects (ewc-object-objects registry)
                                     :protocol 'wayland
                                     :interface 'wl-output
                                     :data output)))

      (ewc-request registry 'bind `((name . ,name)
                                    (interface-len . ,(1+ (length "wl_output")))
                                    (interface . "wl_output")
                                    (version . 4)
                                    (id . ,(ewc-object-id wl-output))))

      (let ((xdg-output (ewc-object-add :objects (ewc-object-objects registry)
                                        :protocol 'xdg-output-unstable-v1
                                        :interface 'zxdg-output-v1)))



        (ewc-request (ewb-outputs-xdg-output-manager outputs)
                     'get-xdg-output `((id . ,(ewc-object-id xdg-output))
                                       (output . ,(ewc-object-id wl-output)))))

      output)))

;; frame needs layout function that handles x y offset
;; (fn surface x y width height)

;; LAYOUT
;; on x y height width update
;; if surface
;;   output-surface  x y width height 4ALL from ewb-output struct

;;   2 cases
;;     ewb-output-init-surface   -> LAYOUT if x y width height
;;     ewb-output-listener 'done -> LAYOUT if x | y | width | height changed & there is surface
;;   + Set Frame parameter if x | y changed

;; 1 layout frame surface on output
(defun ewb-output-layout-frame (output)
  "Returns nil if OUTPUT can not be layed out yet."
  (pcase-let (((cl-struct ewb-output surface 
                          x y width height)
               output))

    (message "Layout frame %s %s %s %s" x y width height) ; DEBUG
    (when (and surface x y width height)
      (ewb-layout surface x y width height)
      (message "Layed out frame!")      ; DEBUG
      )))

;; 2 layout surface on output (with offset)
(defun ewb-output-layout-function (dx dy dheight)
  ;; ewb-layout-on-output ?
  (lambda (object x y width height &optional inner-p)
    "If INNER-P layout in frames inner area, the area occupied by
windows including the minibuffer."
    ;; Offset for menu- and tool-bar
    ;; frame-outer-height could be used instead of dheight
    ;; but is same as frame-inner-height on pgtk. BUG?
    (ewb-layout object (+ x dx) (+ y dy
                                   (if inner-p
                                       (- dheight (frame-inner-height))
                                     0))
                width height)))
;; (setf (frame-parameter frame parameter) value)
;; -> Set ewb-output-layout-surface as layout-surface frame-parameter

;; frame-heights
;; (frame-inner-height)
;; => 1200 1134
;; (frame-outer-height)
;; => 1200 1200
;; (frame-text-height)                       ; text area
;; => 1200 1134
;; (frame-native-height)                     ; on gtk no menu or toolbar
;; => 1200 1134
;; 2nd value is with tool&menu-bar

(defun ewb-output-update (output update)
  ;; Used in ewb-output-listener done
  (pcase-let (((map x y width height name description) update))
    ;; Update output struct
    (when x (setf (ewb-output-x output) x))
    (when y (setf (ewb-output-y output) y))
    (when width (setf (ewb-output-width output) width))
    (when height (setf (ewb-output-height output) height))
    (when name (setf (ewb-output-name output) name))
    (when description (setf (ewb-output-description output) description))
    
    ;; Update layout-surface function
    (when (or x y height)
      (setf (frame-parameter (ewb-output-frame output) 'layout-surface)
            (ewb-output-layout-function (ewb-output-x output)
                                        (ewb-output-y output)
                                        (ewb-output-height output))))

    ;; Update output frame layout
    (when (or x y width height)
      (ewb-output-layout-frame output))))

;;; Layout & surface handling
(defun ewb-layout-init (registry name version _outputs)
  (cl-assert (eql version 1))
  (let ((layout (ewc-object-add :objects (ewc-object-objects registry)
                                :protocol 'emacs-wayland-protocol
                                :interface 'ewp-layout)))
    (setf (ewc-listener layout 'new-surface)
          #'ewb-surface-new)
    (ewc-request registry 'bind `((name . ,name)
                                  (interface-len . ,(1+ (length "ewp_layout")))
                                  (interface . "ewp_layout")
                                  (version . 1)
                                  (id . ,(ewc-object-id layout))))))

(defun ewb-surface-destroy (surface _args)
  (let ((buffer (ewc-object-data surface)))
    (when (and buffer
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (setq ewb-buffer-surface nil)
        (kill-buffer)))))

(defun ewb-surface-update-title (surface args)
  (when-let ((title (alist-get 'title args))
             (buffer (ewc-object-data surface)))
    (with-current-buffer buffer
      (rename-buffer (format "*X %s*" title) 'unique))))

(defun ewb-surface-focus (surface _args)
  (when-let ((buffer (ewc-object-data surface)))
    (select-window (car (alist-get buffer ewb-buffers)))))

(defun ewb-surface-init (objects)
  (setf (ewc-listener-global objects 'emacs-wayland-protocol 'ewp-surface 'destroy)
        #'ewb-surface-destroy)
  (setf (ewc-listener-global objects 'emacs-wayland-protocol 'ewp-surface 'update-title)
        #'ewb-surface-update-title)
  (setf (ewc-listener-global objects 'emacs-wayland-protocol 'ewp-surface 'focus)
        #'ewb-surface-focus))

(defvar ewb-surface-functions (list #'ewb-buffer-init)
  "Abnormal hook. Run if new surface requests a layout.
Each function is passed surface app-id title pid as arguments
The function should return nil if it does not handle this surface.")

(defun ewb-surface-new (object args)
  (message "New surface: %s" args) ; DEBUG
  
  (pcase-let (((map id ('app_id app-id) title pid) args))
    ;; handle update-title and destroy events -> do it once in init

    ;; add buffer and link to surface
    (run-hook-with-args-until-success 'ewb-surface-functions
                                      (ewc-object-add :objects (ewc-object-objects object)
                                                      :protocol 'emacs-wayland-protocol
                                                      :interface 'ewp-surface
                                                      :id id
                                                      ;; :data ?
                                                      )
                                      app-id title pid)))

;;; General layout function
(defun ewb-layout (surface x y width height)
  "Layout a ewp-SURFACE at X Y with WIDTH and HEIGHT."
  (cl-assert (and (ewc-object-p surface)
                  (seq-every-p #'natnump (list x y width height))))

  (message "Trying to layout surface id=%s %s %s %s %s"
           (ewc-object-id surface)
           x y width height) ; DEBUG

  (ewc-request surface 'layout `((x . ,x) (y . ,y)
                                 (width . ,width) (height . ,height))))

(defun ewb-hide (surface)
  (ewc-request surface 'hide))

;;; Buffer
(defvar-local ewb-buffer-surface nil)

(defun ewb-buffer-layout (&optional window)
  "Layout current ewBuffer on current window or WINDOW."
  (pcase-let ((`(,left ,top ,right ,bottom) (window-absolute-body-pixel-edges window)))
    (message "Update layout %s %s %s %s" left top right bottom)
    
    (funcall (frame-parameter nil 'layout-surface)
             ewb-buffer-surface
             left top
             (- right left) (- bottom top)
             t)))

;; Update frame & windows
;; Uses window-size-change-functions
;; -> Runs if window changed size or buffer, or was added to frame.
;;    The global hook runs once per frame with frame as arg.
;;    The buffer-local hook runs once per window with window as arg.
;; => The buffer-local hook does not run on window or buffer hide.
;; Update rule: A ewBuffer can only be shown once!
;;            & Switch it into focus(ed window) if possible.
;; A global Alist from ewBuffer to window keeps track of state.
(defvar ewb-buffers nil "Global Alist from ewBuffer to window.")
;; It is populated lazily on update but could be seeded early by
;; ewb-buffer-init.
;; The different states:
;; (buffer)  | (buffer window) | (buffer window ...)
;; Hidden    | Layed out       | Update in progress

;; This update mechanism has many hours (~6) and iterations. Keep it.
;; It is good :)

(defun ewb-update-window (window)
  "Record a new or changed WINDOW showing an ewBuffer to `ewb-buffers'.
Add buffer-local in `ewb-buffer-mode' to  `window-size-change-functions'."
  (message "Changed win %s" window)     ; DEBUG
  (push window (alist-get (window-buffer window) ewb-buffers)))

(defun ewb-update-buffer (buffer->windows)
  "Apply an BUFFER->WINDOWS element from `ewb-buffers' and return
its current state."
  (let ((buffer (car buffer->windows))
        (windows (cdr buffer->windows)))
    (cond
     ((not windows) buffer->windows)
     ((buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Find next window: Either the selected window or the first live window
        (let ((next (or (car (memq (selected-window) windows))
                        (seq-find #'window-live-p windows)))
              done)
          (unless (eq buffer (window-buffer next))
            (setq next nil))
          
          (message "Next: %s" next)     ; DEBUG
          (if next
              (ewb-buffer-layout next)
            (ewb-hide ewb-buffer-surface))

          (dolist (window windows)
            (unless (or (eq window next)
                        (memq window done)
                        (not (window-live-p window))
                        (not (eq buffer (window-buffer window))))
              (switch-to-next-buffer window)
              (push window done)))

          (cons buffer (when next (list next)))))))))

(defun ewb-update-frame (frame)
  "Update ewBuffers on Frame.
Add globally to `window-size-change-functions'."
  (message "Update: %s" ewb-buffers)    ; DEBUG
  (setq ewb-buffers (delq nil (mapcar #'ewb-update-buffer ewb-buffers)))
  (message "Updated: %s" ewb-buffers))  ; DEBUG

(defun ewb-buffer-kill ()
  "Destroy `ewb-buffer-surface' before killing a ewBuffer.
Add to `kill-buffer-query-functions'."
  (when ewb-buffer-surface
    (ewc-request ewb-buffer-surface 'destroy))
  t)

(defun ewb-buffer-focus (window)
  "Handle focus for a WINDOW showing a `ewb-buffer-surface'.
Add buffer-local to `window-selection-change-functions'."
  (if (eq window (selected-window))
      ;; Focus
      (with-current-buffer (window-buffer window)
        (ewc-request ewb-buffer-surface 'focus))
    ;; Defocus
    (ewc-request (frame-parameter (window-frame window) 'ewb-surface) 'focus)))

(define-derived-mode ewb-buffer-mode nil "X"
  "Major mode for managing wayland buffers.

\\{ewb-buffer-mode-map}"
  ;; Seeded from exwm-mode
  
  ;; Disallow changing the major-mode
  (add-hook 'change-major-mode-hook #'kill-buffer nil t)
  ;; Adapt kill-buffer
  (add-hook 'kill-buffer-query-functions #'ewb-buffer-kill nil t)
  ;; TODO: Redirect events when executing keyboard macros.
  ;; (push `(executing-kbd-macro . ,exwm--kmacro-map)
  ;;       minor-mode-overriding-map-alist)
  (setq buffer-read-only t
        cursor-type nil
        left-margin-width nil
        right-margin-width nil
        left-fringe-width 0
        right-fringe-width 0
        vertical-scroll-bar nil)

  (add-hook 'window-size-change-functions #'ewb-update-window nil t)
  (add-hook 'window-selection-change-functions #'ewb-buffer-focus nil t))

(defun ewb-buffer-init (surface _app-id title _pid)
  (with-current-buffer (generate-new-buffer (format "*X %s*" title))
    (insert "There is only one view of a wayland buffer.")
    (ewb-buffer-mode)
    (setq ewb-buffer-surface surface)
    (setf (ewc-object-data surface) (current-buffer))
    (pop-to-buffer-same-window (current-buffer))))

;;; Init
(defun ewb-start-server ()
  (make-process
   :name "emacs-wayland-server"
   :buffer "*emacs-wayland-server*"
   :command (list (expand-file-name "./ews"))
   :filter (lambda (proc str)
             ;; (message "S: %s" str)
                                        ; DEBUG
             (with-current-buffer (process-buffer proc)
               (goto-char (point-max))
               (save-excursion (insert str))
               (when (re-search-forward (rx "WAYLAND_DISPLAY=" (group (+ (not control))))
                                        nil t)
                 ;; (setf (process-filter proc) #'internal-default-process-filter)
                 (ewb-start-client (match-string 1)))))))

;; Set WAYLAND_DISPLAY inside emacs instead of arg
(defun ewb-start-client (socket)
  (let* ((objects (ewc-connect
                   (ewc-read ("~/s/wayland/ref/wayland/protocol/wayland.xml"
                              wl-display wl-registry wl-output)
                             ;; TODO: Use pkg-config?
                             ;;   pkg-config --variable=pkgdatadir wayland-protocols
                             ("/gnu/store/4xinswliqgki9sxkffh1gg4fdymca0ph-wayland-protocols-1.26/share/wayland-protocols/unstable/xdg-output/xdg-output-unstable-v1.xml"
                              zxdg-output-manager-v1 zxdg-output-v1) ; = all interfaces
                             ("ewp.xml"
                              ewp-layout ewp-surface))
                   (expand-file-name socket (xdg-runtime-dir))))
         (registry (ewc-object-add :objects objects
                                   :protocol 'wayland
                                   :interface 'wl-registry))
         (outputs (ewb-outputs-make)))
    ;; TODO: Move outputs to var? Less encapsulation is  more emacsy.
    
    (ewb-output-init objects)
    (ewb-surface-init objects)
    
    ;; Add global listener
    (setf (ewc-listener registry 'global)
          (pcase-lambda (_object (map name interface version))
            ;; DEBUG
            (message "Global %s %s %s" name interface version)
            ;; (message "outputs %s" outputs)
            (pcase interface
              ("zxdg_output_manager_v1" (setf (ewb-outputs-xdg-output-manager outputs)
                                              (ewb-output-xdg-manager registry name version)))
              ("wl_output" (push (ewb-output-new registry outputs name version)
                                 (ewb-outputs-list outputs)))
              ("ewp_layout" (ewb-layout-init registry name version outputs)))))

    (ewc-request (ewc-object-get 1 objects) 'get-registry `((registry . ,(ewc-object-id registry))))))

(defun ewb-init (&optional server-p)
  ;; Make emacs resize pixelwise
  (setq frame-resize-pixelwise t
        window-resize-pixelwise t)

  (add-hook 'window-size-change-functions #'ewb-update-frame)

  (when server-p                        ; DEBUG
    (ewb-start-server)))

;;; TODO:
;; - Abstract common pattern: ewc-object-add -> objects & ewc-request with object-id in args

(provide 'ewb)
;;; ewb.el ends here
