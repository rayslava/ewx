;;; ewc.el --- A wayland client in elisp:) -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "28.2"))

;;; Objects -> Commentary:
;; => [2022-10-20 Thu]

;; OBJECTS

;; interface -> object
;; ;; the interface is the blueprint; it becomes alive as an object

;; (object) -> data
;; (setf (object) data) | (object data)

;; (ewc-protocol xml . interface) -> (protocol
;;                                    (interface version ((name ue . listener) ...) ((name opcode le . pe) ...))
;;                                    ...) ; compiled ; only listener is rw

;; ;; A listener is an event callback.
;; (setf (ewc-listener protocol interface event) listener)

;; (ewc-object protocol interface) -> #s(ewc-object
;;                                       protocol
;;                                       interface
;;                                       id ; add new object to ewc-objects -> id
;;                                       data     ; only rw field
;;                                       ((name ue . listener) ...) ; shared ; set by name
;;                                       ((name opcode le . pe) ...) ; shared ; looked up by name
;;                                       )

;; (ewc-request object request . (named args)) -> id & opcode & pe -> msg

;; (ewc-event str) -> id & opcode -> lookup object -> ue & listener -> (listener object msg)

;; Seems sound: short & concise :)

;;; Code:
(require 'cl-macs)
(require 'seq)
(require 'map)
(require 'pcase)

(require 'bindat)

;;; Read wayland xml protocols
;; enums are not implemented

;; wayland-scanner implemented as a macro. This makes compiling of the
;; wayland protocols possible, which is mainly relevant for the
;; contained bindat pack and unpack functions.
;; Use it this way:
;; (defvar wayland-protocols
;;   (ewc-read "protocol1.xml"
;;             ("protocol2.xml" interface1 interface3)))

(defmacro ewc-read (&rest protocol)
  "Read wayland PROTOCOLs from  xml files into elisp.
Each PROTOCOL is either a path to a wayland xml protocol
or a list (path interface ...) restricting the interfaces
read to those specified.

This is the elisp version of wayland-scanner."
  ;; (protocol ...)
  `(list ,@(mapcar #'ewc-read-protocol protocol)))

(defvar bindat-raw)
(defvar bindat-idx)

(define-inline ewc-node-name (node)
  (inline-quote
   (intern (string-replace "_" "-" (dom-attr ,node 'name)))))

(defun ewc-read-protocol (protocol &optional select-interfaces)
  (unless (stringp protocol)
    (setq select-interfaces (cdr protocol))
    (setq protocol (car protocol)))

  (let ((protocol (with-current-buffer (find-file-noselect protocol)
                    (libxml-parse-xml-region (point-min) (point-max)))))
    ;; (protocol interface ...)
    `(cons ',(ewc-node-name protocol)
           (list ,@(mapcar #'ewc-read-interface
                           (let ((interfaces (dom-by-tag protocol 'interface)))
                             (if select-interfaces
                                 (map-filter (lambda (key _) (member key select-interfaces)) interfaces)
                               interfaces)))))))

(defun ewc-read-interface (interface)
  ;; (interface version (event ...) (request ...))
  `(list
    ',(ewc-node-name interface)
    ,(string-to-number (dom-attr interface 'version))
    (list ,@(mapcar #'ewc-read-event (dom-by-tag interface 'event)))
    (list ,@(seq-map-indexed #'ewc-read-request (dom-by-tag interface 'request)))))

(defun ewc-read-event (event)
  ;; (event ue . listener)
  `(list ',(ewc-node-name event)
         ,(bindat--toplevel 'unpack (mapcan #'ewc-read-arg (dom-by-tag event 'arg)))))

;; mapcan instead of append & mapcar produces loops

(defun ewc-read-request (request opcode)
  (let ((spec (mapcan #'ewc-read-arg (dom-by-tag request 'arg))))
    ;; (request . pe)
    `(cons ',(ewc-node-name request)
           (cons ,opcode
                 (cons ,(bindat--toplevel 'length spec)
                       ,(bindat--toplevel 'pack spec))))))

;; TODO: Wayland uses cpu endianess. Detect it or make it configurable.
(defun ewc-read-arg (node)
  (let ((name (ewc-node-name node)))
    (pcase (dom-attr node 'type)
      ((and "new_id" (guard (not (dom-attr node 'interface))))
       `((interface-len uint 32 t)
         (interface strz)
         (_ align 4)
         (version uint 32 t)
         (,name uint 32 t)))
      ((or "uint" "object" "new_id")
       `((,name uint 32 t)))
      ("int"
       `((,name sint 32 t)))
      ("string"
       `((,(intern (format "%s-len" name)) uint 32 t)
         (,name strz)
         ;; Hack: If not constructed with list this leads to circular lists due to nconc.
         ;; Have a look at the expansion of this backquote and (elisp) Repeated Expansion.
         ,(list '_ 'align 4)))        
      ((or "fixed" "array" "fd")
       ;; fixed: ((integer sint 24 t) (decimal uint 8 t))
       ;; fd: file descriptor passed via msg_control -> can't be done in elisp
       ;; Use https://github.com/skeeto/bitpack ?
       `((,name not-implemented))))))

(defvar ewc-protocols nil "")

;;; Objects
;; implementing the wayland interfaces.

(cl-defstruct (ewc-object (:constructor ewc-object--make)
                          (:copier nil))
  (protocol nil :type symbol :read-only t)
  (interface nil :type symbol :read-only t)
  (id nil :type integer :read-only t)
  (data nil :read-only nil)
  (events nil :type list :read-only t)
  (requests nil :type list :read-only t))
;; Add version?

(defvar ewc-objects nil
  "List of `ewc-objects' in current session.")

(defun ewc-object (protocol interface &optional data)
  "Generate an object from INTERFACE of PROTOCOL with optional DATA."
  (pcase-let* ((`(,_version ,events ,requests)
                (thread-last
                  ewc-protocols
                  (alist-get protocol)
                  (alist-get interface)))
               (object (ewc-object--make
                        :protocol protocol
                        :interface interface
                        :id (1+ (length ewc-objects))
                        :data data
                        :events events
                        :requests requests)))
    (setq ewc-objects (nconc ewc-objects (list object)))
    object))

;; This sets the listener for all objects.
;; define-inline makes ewc-listener usable with setf.
(define-inline ewc-listener (object event)
  "Return listener for event."
  (inline-quote
   (cdr (alist-get ,event (ewc-object-events ,object)))))

;;; Parse and print wayland wire messages
(defvar ewc-msg-head
  (bindat-type (id uint 32 t)
               (opcode uint 16 t)
               (len uint 16 t)))

(defun ewc-event (str str-len idx)
  "Parse wayland event wire message STRing with STR-LENength starting at IDX
and dispatch to the events listener."
  (pcase-let*
      ((bindat-idx idx)
       (bindat-raw str)
       ((map id opcode _len) (funcall (bindat--type-ue ewc-msg-head)))
       (object (nth (1- id) ewc-objects))
       (`(,_event ,ue . ,listener) (nth opcode (ewc-object-events object))))

    ;; DEBUG
    (message "rx: id=%s opcode=%s" id opcode)

    ;;        DEBUG nil listener should just ignore the message
    ;;        -> Could just not call ue but then something else must
    ;;           advance bindat-idx; use _len?
    (funcall (or listener (lambda (_ msg) (message "msg: %s" msg)))
             object (when ue (funcall ue)))
    
    (unless (= str-len bindat-idx)
      (ewc-event bindat-raw str-len bindat-idx))))

(defun ewc-pack (object request arguments)
  "Return wayland REQUEST wire message for OBJECT with ARGUMENTS."
  (pcase-let*
     ((`(,_ ,opcode ,le . ,pe) (assq request (ewc-object-requests object)))
      (bindat-idx 0)
      (len  (+ 8 (if le
                     (funcall le arguments)
                   0)))
      (bindat-idx 0)
      (bindat-raw (make-string len 0)))

    (funcall (bindat--type-pe ewc-msg-head) `((id . ,(ewc-object-id object))
                                              (opcode . ,opcode)
                                              (len . ,len)))
    (when pe (funcall pe arguments))

    bindat-raw))

;;; wl-display
(defun ewc-filter (_proc str)
  ;; DEBUG
  (with-current-buffer (get-buffer-create "*wayland*")
    (insert str))

  (ewc-event str (length str) 0))

(defun ewc-connect ()
  ;; | ewc-init
  (if (and ewc-objects
           (process-live-p (nth 0 ewc-objects)))
      (message "Emacs wayland client already connected")
    (setq ewc-objects nil)
    (ewc-object  'wayland 'wl-display
                 (make-network-process
                  :name "emacs-wayland-client"
                  :remote "/run/user/1000/wayland-0" ; TODO: Make configurable/ detect.
                  :coding 'binary
                  :filter #'ewc-filter))
    (message "Emacs wayland client connected")))

(defun ewc-request (object request arguments)
  (process-send-string (ewc-object-data (nth 0 ewc-objects)) (ewc-pack object request arguments)))

;; TODO: Add cleanup fn

;;; wl-registry
(defun ewc-get-registry ()
  (let ((registry (ewc-object 'wayland 'wl-registry)))
    ;; Add global listener
    (setf (ewc-listener registry 'global)
          (pcase-lambda (object (map name interface version))
            ;; Add interface to this registries data
            (push `(,name (interface . ,interface) (version . ,version))
                  (ewc-object-data object))))
    ;; Issue the request
    (ewc-request (nth 0 ewc-objects) 'get-registry `((registry . ,(ewc-object-id registry))))
    registry))

(provide 'ewc)
;;; ewc.el ends here
