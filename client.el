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
                               (opcode . 0)
                               (length . 12)
                               (new_id . 3))))

(bindat-unpack
 '((id u32r)
   (opcode u16r)
   (length u16r))
 (with-current-buffer "aa"
   (string-to-unibyte (buffer-substring-no-properties 1 9))))
;; => ((length . 12) (opcode . 0) (id . 2))

(bindat-unpack
 '((id u32r)
   (opcode u16r)
   (length u16r)
   (callback_data u32r))
 (with-current-buffer "aa"
   (string-to-unibyte (buffer-substring-no-properties 1 13))))
;; => ((callback_data . 1100) (length . 12) (opcode . 0) (id . 2))

(with-current-buffer "aa"
  (goto-char (point-min))
  (end-of-line)
  (length (string-to-unibyte (buffer-substring-no-properties 1 (1- (point))))))
;; => 23

(bindat-unpack
 '((id u32r)
   (opcode u16r)
   (length u16r)
   (new_id u32r))
 (with-current-buffer "aa"
   (string-to-unibyte (buffer-substring-no-properties 13 25))))
;; => ((new_id . 2) (length . 12) (opcode . 1) (id . 1))

