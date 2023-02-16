;;; ewl.el --- Emacs wayland layout   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Michael Bauer

;; Author: Michael Bauer <michael-bauer@posteo.de>
;; Keywords: unix
;; Version: 0.1
;; Homepage: https://perma-curious/repo-ewx
;; Package-Requires: ((emacs "28.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs wayland layout integrates wayland surfaces with Emacs.

;; This is accomplished by implementing the Emacs wayland protocol
;; as a wayland client.

;; New surface handling:
;; 1. If a new toplevel surface is created the wayland server issues
;;    an event.
;; 2. A new ewp-surface object is added to the client.
;; 3. The object together with its app-id, title and pid gets passed
;;    to the ewl-surface-functions.
;; 4. The first function on this hook that returns non nil does the
;;    rest of the surface handling.
;;    The default handler treats the surface as an Emacs buffer.

;; The main mechanism to identify a new surface is its pid.

;; Emacs wayland layout also handles wayland outputs (your monitor)
;; and decorates them with an Emacs frame each.

;; If a new output is added to the server, Emacs creates a new frame
;; and layouts it to fill the new output. Additionally a output
;; local layout function is added as frame parameter.

;; The different roles of wayland surfaces are:
;;   1. Emacs frame    = a new frame of the main Emacs
;;   2. Wayland buffer = An arbitrary wayland surface treated as Emacs buffer 
;;   3. Wayland widget = An arbitrary wayland surface embedded into an Emacs buffer
;;   The first two are already implemented.

;; Future output handling ...


;; NEXT: Go over rest and frohlocke ;)
;; ---

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

;; Setup ewl buffer to create/delete/update layout on show/hide/update

;; Output size change -> relayout linked frame

;;; Code:
(require 'ewc)
(require 'xdg)

;;; Helper
(defun add-onetime-hook (hook function)
  "Add FUNCTION to HOOK and remove it after being called once."
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

(cl-defstruct (ewl-outputs (:constructor ewl-outputs-make)
                           (:copier nil))
  (xdg-output-manager nil :type ewc-object)
  (list nil :type list))             ; list of ewl-output TODO: Just have a list & keep xdg-output-manager seperate?

(cl-defstruct (ewl-output (:constructor ewl-output-make)
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
(defun ewl-output-xdg-manager (registry name version)
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
  (defun ewl-output-listener (event)
    (pcase event
      ((or 'logical-position 'logical-size 'name 'description)
       (lambda (_object values)
         (push values update)))
      ('done
       (lambda (object _values)
         (let ((update (apply #'nconc update)))
           (message "Received update: %s" update) ; DEBUG
           (ewl-output-update (ewc-object-data object) update))
         (setq update nil))))))

;; Split in -init and -new to set listeners only once and not for each new output.
(defun ewl-output-init (objects)
  (setf (ewc-listener-global objects 'wayland 'wl-output 'done)
        (ewl-output-listener 'done))
  
  (dolist (event '(logical-position logical-size name description))
    (setf (ewc-listener-global objects 'xdg-output-unstable-v1 'zxdg-output-v1 event)
          (ewl-output-listener event))))

(defun ewl-output-init-surface (output)
  (lambda (surface _app-id _title pid)
    (message "Init with pid %s=%s?" pid (emacs-pid))            ; DEBUG
    (when (eql pid (emacs-pid))
      (message "Init with pid %s!" pid) ; DEBUG
      (setf (ewl-output-surface output) surface)
      ;; frame-parameter is used by ewl-buffer-focus
      ;; TODO: Use frame-parameters for output handling?
      (setf (frame-parameter (ewl-output-frame output) 'ewl-surface) surface)
      ;; Try to layout frame. Succeeds when x y width height are already set.
      (ewl-output-layout-frame output)
      t)))

(defun ewl-output-new (registry outputs name version)
  (cl-assert (eql version 4))

  (let ((output (ewl-output-make)))

    (add-onetime-hook 'ewl-surface-functions (ewl-output-init-surface output))

    (setf (ewl-output-frame output)
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



        (ewc-request (ewl-outputs-xdg-output-manager outputs)
                     'get-xdg-output `((id . ,(ewc-object-id xdg-output))
                                       (output . ,(ewc-object-id wl-output)))))

      output)))

;; frame needs layout function that handles x y offset
;; (fn surface x y width height)

;; LAYOUT
;; on x y height width update
;; if surface
;;   output-surface  x y width height 4ALL from ewl-output struct

;;   2 cases
;;     ewl-output-init-surface   -> LAYOUT if x y width height
;;     ewl-output-listener 'done -> LAYOUT if x | y | width | height changed & there is surface
;;   + Set Frame parameter if x | y changed

;; 1 layout frame surface on output
(defun ewl-output-layout-frame (output)
  "Returns nil if OUTPUT can not be layed out yet."
  (pcase-let (((cl-struct ewl-output surface 
                          x y width height)
               output))

    (message "Layout frame %s %s %s %s" x y width height) ; DEBUG
    (when (and surface x y width height)
      (ewl-layout surface x y width height)
      (message "Layed out frame!")      ; DEBUG
      )))

;; 2 layout surface on output (with offset)
(defun ewl-output-layout-function (dx dy dheight)
  ;; ewl-layout-on-output ?
  (lambda (object x y width height &optional inner-p)
    "If INNER-P layout in frames inner area, the area occupied by
windows including the minibuffer."
    ;; Offset for menu- and tool-bar
    ;; frame-outer-height could be used instead of dheight
    ;; but is same as frame-inner-height on pgtk. BUG?
    (ewl-layout object (+ x dx) (+ y dy
                                   (if inner-p
                                       (- dheight (frame-inner-height))
                                     0))
                width height)))
;; (setf (frame-parameter frame parameter) value)
;; -> Set ewl-output-layout-surface as layout-surface frame-parameter

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

(defun ewl-output-update (output update)
  ;; Used in ewl-output-listener done
  (pcase-let (((map x y width height name description) update))
    ;; Update output struct
    (when x (setf (ewl-output-x output) x))
    (when y (setf (ewl-output-y output) y))
    (when width (setf (ewl-output-width output) width))
    (when height (setf (ewl-output-height output) height))
    (when name (setf (ewl-output-name output) name))
    (when description (setf (ewl-output-description output) description))
    
    ;; Update layout-surface function
    (when (or x y height)
      (setf (frame-parameter (ewl-output-frame output) 'layout-surface)
            (ewl-output-layout-function (ewl-output-x output)
                                        (ewl-output-y output)
                                        (ewl-output-height output))))

    ;; Update output frame layout
    (when (or x y width height)
      (ewl-output-layout-frame output))))

;;; Layout & surface handling
(defun ewl-layout-init (registry name version _outputs)
  (cl-assert (eql version 1))
  (let ((layout (ewc-object-add :objects (ewc-object-objects registry)
                                :protocol 'emacs-wayland-protocol
                                :interface 'ewp-layout)))
    (setf (ewc-listener layout 'new-surface)
          #'ewl-surface-new)
    (ewc-request registry 'bind `((name . ,name)
                                  (interface-len . ,(1+ (length "ewp_layout")))
                                  (interface . "ewp_layout")
                                  (version . 1)
                                  (id . ,(ewc-object-id layout))))))

(defun ewl-surface-destroy (surface _args)
  (let ((buffer (ewc-object-data surface)))
    (when (and buffer
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (setq ewl-buffer-surface nil)
        (kill-buffer)))))

(defun ewl-surface-update-title (surface args)
  (when-let ((title (alist-get 'title args))
             (buffer (ewc-object-data surface)))
    (with-current-buffer buffer
      (rename-buffer (format "*X %s*" title) 'unique))))

(defun ewl-surface-focus (surface _args)
  (when-let ((buffer (ewc-object-data surface)))
    (select-window (car (alist-get buffer ewl-buffers)))))

(defun ewl-surface-init (objects)
  (setf (ewc-listener-global objects 'emacs-wayland-protocol 'ewp-surface 'destroy)
        #'ewl-surface-destroy)
  (setf (ewc-listener-global objects 'emacs-wayland-protocol 'ewp-surface 'update-title)
        #'ewl-surface-update-title)
  (setf (ewc-listener-global objects 'emacs-wayland-protocol 'ewp-surface 'focus)
        #'ewl-surface-focus))

(defvar ewl-surface-functions (list #'ewl-buffer-init)
  "Abnormal hook. Run if new surface requests a layout.
Each function is passed surface app-id title pid as arguments
The function should return nil if it does not handle this surface.")

(defun ewl-surface-new (object args)
  (message "New surface: %s" args) ; DEBUG
  
  (pcase-let (((map id ('app_id app-id) title pid) args))
    ;; handle update-title and destroy events -> do it once in init

    ;; add buffer and link to surface
    (run-hook-with-args-until-success 'ewl-surface-functions
                                      (ewc-object-add :objects (ewc-object-objects object)
                                                      :protocol 'emacs-wayland-protocol
                                                      :interface 'ewp-surface
                                                      :id id
                                                      ;; :data ?
                                                      )
                                      app-id title pid)))

;;; General layout function
(defun ewl-layout (surface x y width height)
  "Layout a ewp-SURFACE at X Y with WIDTH and HEIGHT."
  (cl-assert (and (ewc-object-p surface)
                  (seq-every-p #'natnump (list x y width height))))

  (message "Trying to layout surface id=%s %s %s %s %s"
           (ewc-object-id surface)
           x y width height) ; DEBUG

  (ewc-request surface 'layout `((x . ,x) (y . ,y)
                                 (width . ,width) (height . ,height))))

(defun ewl-hide (surface)
  (ewc-request surface 'hide))

;;; Wayland Buffer
(defvar-local ewl-buffer-surface nil)

(defun ewl-buffer-layout (&optional window)
  "Layout current wayland buffer on current window or WINDOW."
  (pcase-let ((`(,left ,top ,right ,bottom) (window-absolute-body-pixel-edges window)))
    (message "Update layout %s %s %s %s" left top right bottom)
    
    (funcall (frame-parameter nil 'layout-surface)
             ewl-buffer-surface
             left top
             (- right left) (- bottom top)
             t)))

;; Update frame & windows
;; Uses window-size-change-functions
;; -> Runs if window changed size or buffer, or was added to frame.
;;    The global hook runs once per frame with frame as arg.
;;    The buffer-local hook runs once per window with window as arg.
;; => The buffer-local hook does not run on window or buffer hide.
;; Update rule: A wayland buffer can only be shown once!
;;            & Switch it into focus(ed window) if possible.
;; A global Alist from wayland buffer to window keeps track of state.
(defvar ewl-buffers nil "Global Alist from wayland buffer to window.")
;; It is populated lazily on update but could be seeded early by
;; ewl-buffer-init.
;; The different states:
;; (buffer)  | (buffer window) | (buffer window ...)
;; Hidden    | Layed out       | Update in progress

;; This update mechanism has many hours (~6) and iterations. Keep it.
;; It is good :)

(defun ewl-update-window (window)
  "Record a new or changed WINDOW showing an wayland buffer to `ewl-buffers'.
Add buffer-local in `ewl-buffer-mode' to  `window-size-change-functions'."
  (message "Changed win %s" window)     ; DEBUG
  (push window (alist-get (window-buffer window) ewl-buffers)))

(defun ewl-update-buffer (buffer->windows)
  "Apply an BUFFER->WINDOWS element from `ewl-buffers' and return
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
              (ewl-buffer-layout next)
            (ewl-hide ewl-buffer-surface))

          (dolist (window windows)
            (unless (or (eq window next)
                        (memq window done)
                        (not (window-live-p window))
                        (not (eq buffer (window-buffer window))))
              (switch-to-next-buffer window)
              (push window done)))

          (cons buffer (when next (list next)))))))))

(defun ewl-update-frame (_frame)
  "Update ewluffers on Frame.
Add globally to `window-size-change-functions'."
  (message "Update: %s" ewl-buffers)    ; DEBUG
  (setq ewl-buffers (delq nil (mapcar #'ewl-update-buffer ewl-buffers)))
  (message "Updated: %s" ewl-buffers))  ; DEBUG

(defun ewl-buffer-kill ()
  "Destroy `ewl-buffer-surface' before killing a ewluffer.
Add to `kill-buffer-query-functions'."
  (when ewl-buffer-surface
    (ewc-request ewl-buffer-surface 'destroy))
  t)

(defun ewl-buffer-focus (window)
  "Handle focus for a WINDOW showing a `ewl-buffer-surface'.
Add buffer-local to `window-selection-change-functions'."
  (if (eq window (selected-window))
      ;; Focus
      (with-current-buffer (window-buffer window)
        (ewc-request ewl-buffer-surface 'focus))
    ;; Defocus
    (ewc-request (frame-parameter (window-frame window) 'ewl-surface) 'focus)))

(define-derived-mode ewl-buffer-mode nil "X"
  "Major mode for managing wayland buffers.

\\{ewl-buffer-mode-map}"
  ;; Seeded from exwm-mode
  
  ;; Disallow changing the major-mode
  (add-hook 'change-major-mode-hook #'kill-buffer nil t)
  ;; Adapt kill-buffer
  (add-hook 'kill-buffer-query-functions #'ewl-buffer-kill nil t)
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

  (add-hook 'window-size-change-functions #'ewl-update-window nil t)
  ;; (add-hook 'window-selection-change-functions #'ewl-buffer-focus nil t) TODO: Waiting for input handling
  )

(defun ewl-buffer-init (surface _app-id title _pid)
  (with-current-buffer (generate-new-buffer (format "*X %s*" title))
    (insert "There is only one view of a wayland buffer.")
    (ewl-buffer-mode)
    (setq ewl-buffer-surface surface)
    (setf (ewc-object-data surface) (current-buffer))
    (pop-to-buffer-same-window (current-buffer))))

;;; "Floating"
;; Hack for talk

(defun ewl-buffer-float (buffer x y width height)
  (when-let ((window (car (alist-get buffer ewl-buffers))))
    (switch-to-next-buffer window))
  (redisplay)
  (with-current-buffer buffer
    (funcall (frame-parameter nil 'layout-surface)
             ewl-buffer-surface x y width height t)))

;;; Init
(defun ewl-start-server ()
  (make-process
   :name "emacs-wayland-server"
   :buffer " *emacs-wayland-server*"
   :command (list (expand-file-name "./ews"))
   :filter (lambda (proc str)
             (with-current-buffer (process-buffer proc)
               (goto-char (point-max))
               (save-excursion (insert str))
               (when (re-search-forward (rx "WAYLAND_DISPLAY=" (group (+ (not control))))
                                        nil t)
                 (setf (process-filter proc) #'ignore)
                 (ewl-start-client (match-string 1)))))))

(eval-and-compile
  (defun ewl-get-protocol (library protocol)
    "Use pkg-config to get PROTOCOL from LIBRARY."
    (let ((path (string-trim
                 (shell-command-to-string
                  (format "pkg-config --silence-errors --variable=pkgdatadir %s" library)))))
      (if (string= "" path)
          (error "pkg-config could not find %s" library)
        (expand-file-name protocol path)))))

(defmacro ewl-read-protocols (&rest protocols)
  "Variant of `ewc-read' that uses pkg-config via
`ewl-get-protocol' to resolve PROTOCOLS.

Each PROTOCOL is either a path to a wayland xml protocol,
a list (path interface ...) restricting the interfaces read to
those specified or a list (library protocol interface ...) that
looks up protocol in library."
  (cons 'ewc-read
        (mapcar (lambda (protocol)
                  (pcase-let ((`(,1st ,2nd . ,rst) (ensure-list protocol)))
                    (if (stringp 2nd)
                        (cons (ewl-get-protocol 1st 2nd) rst)
                      protocol)))
                protocols)))

;; Set WAYLAND_DISPLAY inside emacs instead of arg
(defun ewl-start-client (socket)
  (let* ((objects (ewc-connect
                   (ewl-read-protocols
                    ("wayland-client" "wayland.xml"
                     wl-display wl-registry wl-output)
                    ("wayland-protocols" "unstable/xdg-output/xdg-output-unstable-v1.xml"
                     zxdg-output-manager-v1 zxdg-output-v1) ; = all interfaces
                    ("ewp.xml"
                     ewp-layout ewp-surface)) ; = all interfaces
                   (expand-file-name socket (xdg-runtime-dir))))
         (registry (ewc-object-add :objects objects
                                   :protocol 'wayland
                                   :interface 'wl-registry))
         (outputs (ewl-outputs-make)))
    ;; TODO: Move outputs to var? Less encapsulation is  more emacsy.
    
    (ewl-output-init objects)
    (ewl-surface-init objects)
    
    ;; Add global listener
    (setf (ewc-listener registry 'global)
          (pcase-lambda (_object (map name interface version))
            ;; DEBUG
            (message "Global %s %s %s" name interface version)
            ;; (message "outputs %s" outputs)
            (pcase interface
              ("zxdg_output_manager_v1" (setf (ewl-outputs-xdg-output-manager outputs)
                                              (ewl-output-xdg-manager registry name version)))
              ("wl_output" (push (ewl-output-new registry outputs name version)
                                 (ewl-outputs-list outputs)))
              ("ewp_layout" (ewl-layout-init registry name version outputs)))))

    (ewc-request (ewc-object-get 1 objects) 'get-registry `((registry . ,(ewc-object-id registry))))))

(defun ewl-init (&optional server-p)
  ;; Make emacs resize pixelwise
  (setq frame-resize-pixelwise t
        window-resize-pixelwise t)

  (add-hook 'window-size-change-functions #'ewl-update-frame)

  (when server-p                        ; DEBUG
    (ewl-start-server)))

;;; TODO:
;; - Abstract common pattern: ewc-object-add -> objects & ewc-request with object-id in args

(provide 'ewl)
;;; ewl.el ends here
