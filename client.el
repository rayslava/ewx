;; -*- lexical-binding: t; -*-
(setq wtproc
      (make-network-process
       :name "wayland-0"
       :remote "/run/user/1000/wayland-0"
       :coding 'binary
       :buffer "aa"))

(bindat-pack
 ;; spec
 '((id u32r)
   (length u16r)
   (opcode u16r)
   (data vec (eval (- (bindat-get-field struct 'length)
                      8))))
 ;; struct
 '((id . 1)
   (length . 12)
   (opcode . 0)
   (data . 2)))

(process-send-string wtproc (bindat-pack
                             ;; spec
                             '((id u32r)
                               (opcode u16r)
                               (length u16r)
                               (new_id u32r))
                             ;; struct
                             '((id . 1)
                               (opcode . 1) ; get_registry
                               (length . 12)
                               (new_id . 2))))

(bindat-unpack
 '((id u32r)
   (opcode u16r)
   (length u16r))
 (with-current-buffer "aa"
   (string-to-unibyte (buffer-substring-no-properties 1 9))))
;; => ((length . 28) (opcode . 0) (id . 2))

(bindat-unpack
 '((id u32r)
   (opcode u16r)
   (length u16r)
   (name u32r)
   (str-length u32r)
   (interface strz (str-length))
   (align 4)
   (version u32r))
 (with-current-buffer "aa"
   (string-to-unibyte (buffer-substring-no-properties 1 29))))
;; =>
;; ((version . 1)
;;  (interface . "wl_shm")
;;  (str-length . 7)
;;  (name . 1)
;;  (length . 28)
;;  (opcode . 0)
;;  (id . 2))

(bindat-unpack
 '((id u32r)
   (opcode u16r)
   (length u16r)
   (name u32r)
   (str-length u32r)
   (interface strz (str-length))
   (align 4)
   (version u32r))
 (with-current-buffer "aa"
   (string-to-unibyte (buffer-substring-no-properties 29 (+ 29 28)))))
;; =>
;; ((version . 2)
;;  (interface . "wl_drm")
;;  (str-length . 7)
;;  (name . 2)
;;  (length . 28)
;;  (opcode . 0)
;;  (id . 2))

;;; impl: ewc.el a wayland client in elisp:)
(defvar ewc-header
  '((id u32r)
    (opcode u16r)
    (length u16r)))

;; parse
(bindat-unpack ewc-header str idx)
-> id opcode length
(bindat-unpack (lookup msg-spec id opcode) str 4)
idx = (+ idx length) | finished if = (length str) ; make rest an error for now

(defvar ewc-objects nil
  ;; "Alist object ID to object name in current session."
  "List of objects (name) in current session.") ; Use nconc to add new?
;; Next is seeded from protocol xml
(defvar ewc-protocols nil
  "
Tree protocol-name->interface-name->events->((event-name . bindat-spec) ...)
                                  ->requests->((request-name . bindat-spec) ...)")
(defvar ewc-callbacks|listeners nil
  "Alist object name to list/vector of event callbacks")

(defun ewc-parse (str str-len idx)
  ;; header
  (pcase-let (((map id opcode len) (bindat-unpack ewc-header str idx)))
    (bindat-unpack
     (seq-elt (map-elt ewc-events
                       (seq-elt ewc-objects id))
              opcode)
     str
     (+ idx 8))  ; -> call callback
    (let ((idx (+ idx len)))
      (unless (= str-len idx)
        (ewc-parse str str-len idx)))))

;; filter
(lambda (proc str)
  (ewc-parse str (length str) 0))

;; ewc-protocols
;; (protocol-name
;;  (interface-name
;;   (events
;;    (event-name . spec)
;;    ...)
;;   (requests
;;    (request-name . spec)
;;    ...))
;;  ...)

(defun ewc-protocol-read (xml-protocol-file)
  ;; This is wayland-scanner
  (pcase-let* ((node (with-current-buffer (find-file-noselect xml-protocol-file)
                       (libxml-parse-xml-region (point-min) (point-max))))
               (`(,protocol . ,interfaces) (ewc-protocol node)))
    (setf (map-elt ewc-protocols protocol) interfaces)))

(ewc-protocol-read "~/s/wayland/ref/wayland/protocol/wayland.xml")
(ewc-protocol-read "~/s/wayland/ewp.xml")
;; =>
;; (emacs_wayland_protocol
;;  (layout
;;   (events
;;    (layout-new-window
;;     (current_output u32r)
;;     (usable_width u32r)
;;     (usable_height u32r)
;;     (tags u32r)
;;     (title u32r)
;;     (application u32r)
;;     (pid u32r)))
;;   (requests
;;    (layout-window
;;     (x u32r)
;;     (y u32r)
;;     (width u32r)
;;     (height u32r)))))

(defsubst ewc-node-name (node)
  (intern (string-replace "_" "-" (dom-attr node 'name))))

(defun ewc-protocol (node)
  ;; (protocol interface... ...)
  (cons (ewc-node-name node)
        (mapcar #'ewc-protocol-interface (dom-by-tag node 'interface))))

(defun ewc-protocol-interface (node)
  ;; (interface event... request... ...)
  (list (ewc-node-name node)
        (cons 'events
              (mapcar #'ewc-protocol-msg (dom-by-tag node 'event)))
        (cons 'requests
              (mapcar #'ewc-protocol-msg (dom-by-tag node 'request)))))

(defun ewc-protocol-msg (node)
  ;; (event | request name arg ...)
  (cons (ewc-node-name node)
        (mapcan #'ewc-protocol-arg (dom-by-tag node 'arg))))

(defun ewc-protocol-arg (node)
  ;; (arg name type ...)
  (let ((name (ewc-node-name node)))
    (pcase (dom-attr node 'type)
      ((or "uint" "object" "new_id")
       `((,name u32r)))
      ("string"
       `((str-len u32r)
         (,name strz (str-len))
         (align 4)))
      ((or "int" "fixed" "array" "fd" "enum")
       `((,name not-implemented))))))
