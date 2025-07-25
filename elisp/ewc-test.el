;;; Test  -*- lexical-binding: t; -*-
;; Experimentar
;; (setq ewc-protocols
;;       (ewc-read "~/s/wayland/ref/wayland/protocol/wayland.xml"
;;                 "~/s/wayland/ewp.xml"))

;; Reset
(delete-process (ewc-object-data (nth 0 ewc-objects)))

;; Setup
(ewc-connect)

ewc-objects
;; (#s(ewc-object wayland wl-display 1
;;                #<process emacs-wayland-client> ((sync 0 ... closure ... ... ...) (get-registry 1 ... closure ... ... ...)) nil))

(ewc-get-registry)

(ewc-object-data (gethash 2 (ewc-objects-table ewc-objects)))
;; =>
;; ((12 (interface . "wl_output") (version . 4))
;; (11 (interface . "ewp_layout") (version . 1))
;; (10 (interface . "wl_seat") (version . 8))
;; (9 (interface . "org_kde_kwin_server_decoration_manager") (version . 1))
;; (8 (interface . "zxdg_decoration_manager_v1") (version . 1))
;; (7 (interface . "xdg_wm_base") (version . 3))
;; (6 (interface . "wl_data_device_manager") (version . 3))
;; (5 (interface . "wl_subcompositor") (version . 1))
;; (4 (interface . "wl_compositor") (version . 5))
;; (3 (interface . "zwp_linux_dmabuf_v1") (version . 4))
;; (2 (interface . "wl_drm") (version . 2))
;; (1 (interface . "wl_shm") (version . 1)))

;; Move to ewm.el?
;; or this is ewc-XXX-bind = bind an object from the registry
(ewc-object 'emacs-wayland-protocol 'ewp-layout nil)

(ewc-request (gethash 2 (ewc-objects-table ewc-objects))
             'bind `((name . 11)
                     (interface-len . ,(1+ (length "ewp_layout")))
                     (interface . "ewp_layout")
                     (version . 1)
                     (id . 3)))

;; (ewc-request (nth 2 ewc-objects) 'layout-window '((x . 0)
;;                                                   (y . 0)
;;                                                   (width . 0)
;;                                                   (height . 0)))


(setf (ewc-listener (gethash 3 (ewc-objects-table ewc-objects)) 'new-surface)
      (lambda (object new-object)
        ;; new-object = (map id app-id title pid)
        (message "New ewp-surface: %s" new-object)
        (ewc-object 'emacs-wayland-protocol 'ewp-surface new-object
                    (alist-get 'id new-object))))

(ewc-request (gethash 4278190080 (ewc-objects-table ewc-objects))
             'layout '((x . 20)
                       (y . 20)
                       (width . 800)
                       (height . 400)))

(ewc-request (gethash 4278190080 (ewc-objects-table ewc-objects))
             'layout '((x . 50)
                       (y . 50)
                       (width . 200)
                       (height . 400)))

;;; Test wl_output
(ewc-object 'wayland 'wl-output nil)

(ewc-request (nth 1 ewc-objects) 'bind `((name . 12)
                                         (interface-len . ,(1+ (length "wl_output")))
                                         (interface . "wl_output")
                                         (version . 4)
                                         (id . 3)))

;; 2 wl_output s
(ewc-object 'wayland 'wl-output nil)
(ewc-object 'wayland 'wl-output nil)

(ewc-request (nth 1 ewc-objects) 'bind `((name . 12)
                                         (interface-len . ,(1+ (length "wl_output")))
                                         (interface . "wl_output")
                                         (version . 4)
                                         (id . 4)))

(ewc-request (nth 1 ewc-objects) 'bind `((name . 13)
                                         (interface-len . ,(1+ (length "wl_output")))
                                         (interface . "wl_output")
                                         (version . 4)
                                         (id . 5)))
