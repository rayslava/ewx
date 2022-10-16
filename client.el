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
;; Next two are seeded from protocol xml
(defvar ewc-events nil
  "Alist object name to list/vector of event data spec")
(defvar ewc-requests nil
  "Alist object name to list/vector of request data spec")
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
