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

;; Things:
;; protocols, objects, wayland wire messages, helper

;; Objects created from the same protocols share the same listeners.
;; ewc-objects is the runtime struct that contains
;; - the list of objects
;; - the next new-id
;; - the protocols in use

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
                                 (seq-filter
                                  (lambda (interface)
                                    (member (ewc-node-name interface) select-interfaces))
                                  interfaces)
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

;;; Objects
;; implementing the wayland interfaces.

(cl-defstruct (ewc-object (:constructor ewc-object-make)
                          (:copier nil))
  "A client side object implementing a wayland interface."
  (protocol nil :type symbol :read-only t)
  (interface nil :type symbol :read-only t)
  (id nil :type integer :read-only t)
  (data nil :read-only nil)
  (events nil :type list :read-only t)
  (requests nil :type list :read-only t))
;; Add version?

(cl-defstruct (ewc-objects (:constructor ewc-objects-make)
                           (:copier nil))
  "All objects in the current session.
new-id is the id of the next client initiated object.
table is a hash table from object id to `ewc-object'.
protocols is an alist from `ewc-read'."
  (new-id 1 :type integer)
  (table (make-hash-table) :type hash-table :read-only t)
  (protocols nil :type list :read-only t))

(define-inline ewc-object-get (id objects)
  "Get object with ID from OBJECTS an `ewc-objects' struct."
  (inline-quote (gethash ,id (ewc-objects-table ,objects))))

(defun ewc-object-add (&rest args)
  "Add a new object implementing INTERFACE of PROTOCOL to OBJECTS
with optional ID or DATA.

Returns the newly created object.

OBJECTS is an `ewc-objects' struct.
PROTOCOL and INTERFACE are symbols.
DATA can be any value and ID is uint32.

Use optional ID for server initiated objects.

\(fn &key OBJECTS PROTOCOL INTERFACE ID DATA)"
  (pcase-let* (((map :objects :protocol :interface :id :data)
                args)
               (`(,_version ,events ,requests)
                (thread-last
                  (ewc-objects-protocols objects)
                  (alist-get protocol)
                  (alist-get interface)))
               (id (or id (ewc-objects-new-id objects)))
               (object (ewc-object-make
                        :protocol protocol
                        :interface interface
                        :id id
                        :data data
                        :events events
                        :requests requests)))
    (puthash id object (ewc-objects-table objects))
    (cl-incf (ewc-objects-new-id objects))
    object))

;; This sets the listener for all objects created from the same protocols.
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

(defun ewc-event (objects str str-len idx)
  "Parse wayland event wire message STRing with STR-LENgth starting at IDX.
Lookup the object id in OBJECTS and dispatch to the events listener."
  (pcase-let*
      ((bindat-idx idx)
       (bindat-raw str)
       ((map id opcode _len) (funcall (bindat--type-ue ewc-msg-head)))
       (object (ewc-object-get id objects))
       (`(,_event ,ue . ,listener) (nth opcode (ewc-object-events object))))

    ;; DEBUG
    (message "rx: id=%s opcode=%s" id opcode)

    ;;        DEBUG nil listener should just ignore the message
    ;;        -> Could just not call ue but then something else must
    ;;           advance bindat-idx; use _len?
    (funcall (or listener (lambda (_ msg) (message "msg: %s" msg)))
             object (when ue (funcall ue)))
    
    (unless (= str-len bindat-idx)
      (ewc-event objects bindat-raw str-len bindat-idx))))

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
(defun ewc-filter (objects)
  (lambda (_proc str)
    ;; DEBUG
    (with-current-buffer (get-buffer-create "*wayland*")
      (insert str))

    (ewc-event objects str (length str) 0)))

(defun ewc-connect (protocols &optional socket)
  "Connect to wayland SOCKET using PROTOCOLS.
SOCKET defaults to the value of WAYLAND_DISPLAY.
Returns a `ewc-objects' struct with wl-display as object 1."
  ;; | ewc-init
  (let ((objects (ewc-objects-make :protocols protocols)))
    (ewc-object-add
     :objects objects
     :protocol 'wayland
     :interface 'wl-display
     :data (make-network-process
            :name "emacs-wayland-client"
            :remote (or socket (getenv "WAYLAND_DISPLAY"))
            :coding 'binary
            :filter (ewc-filter objects)))
    objects))

(defun ewc-request (objects object request arguments)
  "Issue REQUEST with ARGUMENTS of OBJECT
OBJECT can be an `ewc-object' struct or an id.
OBJECTS is an `ewc-objects' struct."
  (process-send-string (ewc-object-data (ewc-object-get 1 objects))
                       (ewc-pack
                        (cond
                         ((ewc-object-p object) object)
                         ((natnump object) (ewc-object-get object objects))
                         (t (error "Wrong object %s" object)))
                        request
                        arguments)))

;; TODO: Add cleanup fn

;;; wl-registry
(defun ewc-get-registry (objects)
  (let ((registry (ewc-object-add :objects objects
                                  :protocol 'wayland
                                  :interface 'wl-registry)))
    ;; Add global listener
    (setf (ewc-listener registry 'global)
          (pcase-lambda (object (map name interface version))
            ;; Add interface to this registries data
            (push `(,name (interface . ,interface) (version . ,version))
                  (ewc-object-data object))))
    ;; Issue the request
    (ewc-request objects 1 'get-registry `((registry . ,(ewc-object-id registry))))
    registry))

(provide 'ewc)
;;; ewc.el ends here
