;;; ewc.el --- A wayland client in elisp :)   -*- lexical-binding: t; -*-

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

;; This is a wayland client implementation in elisp.

;; If you want to learn more about the wayland protocols, server and
;; clients have a look at this book: https://wayland-book.com/

;; In short: Wayland is a set of protocols that are implemented by a
;; server (the wayland compositor) and clients that interact with
;; the server through the protocols.

;; A wayland protocol is written as xml and defines a set of
;; interfaces.
;; The interface can contain events and requests and acts as a
;; blueprint. It becomes alive as an object in the client (or server).

;; server -event-> client
;; client -request-> server

;; ewc-read tranlates the xml protocol into elisp:
;; (ewc-read "protocol-1.xml"            -> (...
;;           ("protocol-2.xml"               (protocol-2
;;            interface-1 interface-3))       (interface-1
;;                                             version                  
;;                                             ((event proc . listener) ...)
;;                                             ((request . proc) ...))))

;; objects are implemented as ewc-object struct with:
;;   protocol and interface name
;;   numeric id and data slot
;;   they also link to their events, requests and the global objects

;; (ewc-object-data object) -> data
;; (setf (ewc-object-data object) data)

;; The global objects, the state of the wayland client, are kept in
;; an ewc-objects struct together with the protocols.

;; New objects are added wiht ewc-object-add.

;; A listener is an event callback:
;; (setf (ewc-listener object event) listener)

;; A request is issued with:
;; (ewc-request object request . arguments)

;; The initial client setup:
;; (ewc-connect) -> global-objects
;; (ewc-get-registry global-objects) -> registry-object

;;; Code:
(require 'cl-macs)
(require 'seq)
(require 'map)
(require 'pcase)
(require 'subr-x)

(require 'bindat)
(require 'dom)

;;; Read wayland xml protocols
;; enums are not implemented

;; wayland-scanner implemented as a macro. This makes compiling of the
;; wayland protocols possible, which is mainly relevant for the
;; contained bindat pack and unpack functions.
;; This needs the bindat shipped since emacs 28.2

;; Use it this way:
;; (defvar wayland-protocols
;;   (ewc-read "protocol1.xml"
;;             ("protocol2.xml" interface1 interface3)))

;; We use bindat internals
;; -> bindats state vars need to be defined
(defvar bindat-raw)
(defvar bindat-idx)

(defmacro ewc-read (&rest protocol)
  "Read wayland PROTOCOLs from  xml files into elisp.
Each PROTOCOL is either a path to a wayland xml protocol
or a list (path interface ...) restricting the interfaces
read to those specified.

This is the elisp version of wayland-scanner."
  ;; (protocol ...)
  `(progn (defvar bindat-idx)
          (list ,@(mapcar #'ewc-read-protocol protocol))))

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
  (requests nil :type list :read-only t)
  (objects nil :type ewc-objects :read-only t))
;; Add version?

(cl-defstruct (ewc-objects (:constructor ewc-objects-make)
                           (:copier nil))
  "Keep the current state.
new-id is the id of the next client initiated object.
table contains the list of `ewc-object's keyed by their id.
The protocols in use are a alist as returned by `ewc-read'."
  (new-id 0 :type integer)
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
               (id (or id (cl-incf (ewc-objects-new-id objects))))
               (object (ewc-object-make
                        :protocol protocol
                        :interface interface
                        :id id
                        :data data
                        :events events
                        :requests requests
                        :objects objects)))
    (puthash id object (ewc-objects-table objects))
    (message "Added object id=%s interface=%s" id interface) ; DEBUG
    object))

;; Objects created from the same protocols share the same listeners.

;; This sets the listener for all objects created from the same protocols.
;; define-inline makes ewc-listener usable with setf.
(define-inline ewc-listener (object event)
  "Return listener for event."
  (inline-quote
   (cdr (alist-get ,event (ewc-object-events ,object)))))

;; TODO: In flux; Other method to set listener without object
;;       Needed in ewl for output
;; cl-defmethod would be nicer, but how to combine with inline?
(define-inline ewc-listener-global (objects protocol interface event)
  "Return listener for EVENT of INTERFACE in PROTOCOL used in
OBJECTS a ewc-objects struct."
  (inline-quote
   (thread-last
     (ewc-objects-protocols ,objects)
     (alist-get ,protocol)
     (alist-get ,interface)
     (nth 1)
     (alist-get ,event)
     (cdr))))

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
       (bindat-TMP testing in
;; 
raw str)
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

(defun ewc-request (object request &optional arguments)
  "Issue REQUEST with ARGUMENTS of OBJECT an `ewc-object'."
  (message "Sending request %s to %s" request (ewc-object-id object)) ; DEBUG
  (process-send-string (ewc-object-data ; wl-display
                        (ewc-object-get 1 (ewc-object-objects object)))
                       (ewc-pack object request arguments)))

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
    (ewc-request (ewc-object-get 1 objects) 'get-registry `((registry . ,(ewc-object-id registry))))
    registry))

;; TODO: Is ewc-get-registry used? 
;; MAYBE: Abstract common pattern, see ewl.el, instead?

(provide 'ewc)
;;; ewc.el ends here
