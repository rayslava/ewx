;;; ewl-workspace.el --- Workspace management for EWX  -*- lexical-binding: t -*-

;; Copyright (C) 2023  Michael Bauer
;;           (C) 2025  Slava Barinov

;; Author: Slava Barinov <rayslava@rayslava.com>
;; Keywords: unix
;; Version: 0.1
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

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup ewl-workspace nil
  "Workspace management for EWX."
  :group 'ewl)

(defcustom ewl-workspace-number 4
  "Default number of workspaces per output."
  :type 'integer
  :group 'ewl-workspace)

(defcustom ewl-workspace-show-all-buffers nil
  "Non-nil to show buffers from other workspaces in buffer lists."
  :type 'boolean
  :group 'ewl-workspace)

(defcustom ewl-workspace-switch-hook nil
  "Normal hook run after switching workspace."
  :type 'hook
  :group 'ewl-workspace)

;;; Variables

;; Legacy workspace system (moved from ewl.el)
(defvar ewl-output-workspaces (make-hash-table)
  "Hash table mapping output-id to current workspace buffer.")
(defvar ewl-frame-workspaces (make-hash-table :weakness 'key)
  "Hash table mapping frame to its workspace buffer.")
(defvar ewl-workspace-frames (make-hash-table :weakness 'value)
  "Hash table mapping workspace buffer to its frame.")

;; New workspace switching system
(defvar ewl-workspace-list (make-hash-table)
  "Hash table mapping output-id to list of workspace frames.")
(defvar ewl-workspace-current (make-hash-table)
  "Hash table mapping output-id to current workspace frame.")
(defvar ewl-workspace-current-index (make-hash-table)
  "Hash table mapping output-id to current workspace index (0-based).")
(defvar ewl-workspace-surface-assignments (make-hash-table)
  "Hash table mapping surface to (output-id . workspace-index).")

;;; Helper Functions

(defun ewl-workspace-interactive-session-active-p ()
  "Return t if user is in an interactive session that should not be interrupted."
  (or (active-minibuffer-window)          ; Any minibuffer interaction
      (> (recursion-depth) 0)))           ; Recursive edit session

;;; Legacy Workspace Functions (moved from ewl.el)

(defun ewl-workspace-output-set-workspace (output-id buffer)
  "Set the workspace buffer for OUTPUT-ID."
  (puthash output-id buffer ewl-output-workspaces)
  (message "Set workspace for output %d to buffer %s" output-id (buffer-name buffer)))

(defun ewl-workspace-bind-workspace-to-frame (frame workspace)
  "Exclusively bind WORKSPACE buffer to FRAME."
  (puthash frame workspace ewl-frame-workspaces)
  (puthash workspace frame ewl-workspace-frames)
  (message "Bound workspace %s exclusively to frame %s" (buffer-name workspace) frame))

(defun ewl-workspace-output-get-workspace (output-id)
  "Get the current workspace buffer for OUTPUT-ID, creating one if needed."
  (or (gethash output-id ewl-output-workspaces)
      (let ((workspace (generate-new-buffer (format "*Output-%d-Desktop*" output-id))))
        (with-current-buffer workspace
          (insert (format "=== Output %d Independent Desktop ===\n\n" output-id))
          (insert (format "This is an INDEPENDENT desktop for Output %d.\n" output-id))
          (insert (format "Output %d has its own isolated workspace.\n\n" output-id))
          (insert "Features:\n")
          (insert "- Independent buffer management\n")
          (insert "- Frame-local workspace isolation\n")
          (insert "- Separate from other monitors\n\n")
          (insert (format "Current time: %s\n" (current-time-string)))
          (insert (format "Frame: This workspace is bound to Output %d frame\n\n" output-id))
          (insert "Try opening different files or buffers on each monitor!\n")
          ;; Make buffer locally frame-bound
          (setq-local ewl-bound-output-id output-id)
          (setq-local ewl-frame-local-workspace t))
        (puthash output-id workspace ewl-output-workspaces)
        workspace)))

(defun ewl-workspace-restore-frame-workspace (frame)
  "Restore FRAME's exclusive workspace, preventing cross-frame contamination."
  (unless (ewl-workspace-interactive-session-active-p)
    (let ((workspace (gethash frame ewl-frame-workspaces)))
      (when workspace
        ;; Don't automatically switch to workspace on focus events
        ;; This prevents overriding user buffer choice when clicking between displays
        ;; The workspace is still bound to the frame but not forced on every focus
        nil))))

(defun ewl-workspace-maintain-workspace-isolation ()
  "Hook to maintain workspace isolation across all frames."
  (unless (ewl-workspace-interactive-session-active-p)
    (dolist (frame (frame-list))
      (let ((workspace (gethash frame ewl-frame-workspaces)))
        (when (and workspace
                   (frame-live-p frame)
                   (not (eq (selected-frame) frame))) ; Don't interfere with current frame
          ;; Ensure non-current frames show their exclusive workspaces
          (with-selected-frame frame
            (when (not (eq (current-buffer) workspace))
              (set-window-buffer (selected-window) workspace))))))))

;;; New Workspace Switching System

(defun ewl-workspace-assign-all-frames ()
  "Auto-assign all existing frames to appropriate outputs."
  (dolist (frame (frame-list))
    (unless (or (frame-parameter frame 'ewl-output-id)
                (gethash frame ewl-workspace-frame-assignments))
      ;; Assign frameless frames to output 0 by default
      (ewl-workspace-assign-frame-to-output frame 0)))
  (message "Auto-assigned %d frames to workspace management" (length (frame-list))))

(defun ewl-workspace-init-output (output-id frame)
  "Initialize workspaces for OUTPUT-ID with the given primary FRAME."
  (let ((workspace-frames (make-list ewl-workspace-number frame)))
    ;; For now, all workspaces use the same frame
    ;; TODO: Create separate frames per workspace if needed
    (puthash output-id workspace-frames ewl-workspace-list)
    (puthash output-id frame ewl-workspace-current)
    (puthash output-id 0 ewl-workspace-current-index)
    ;; Ensure all frames are assigned to workspaces
    (ewl-workspace-assign-all-frames)
    (message "Initialized %d workspaces for output %d" ewl-workspace-number output-id)))

(defun ewl-workspace-get-current-index (output-id)
  "Get current workspace index for OUTPUT-ID."
  (gethash output-id ewl-workspace-current-index 0))

(defun ewl-workspace-get-current-frame (output-id)
  "Get current workspace frame for OUTPUT-ID."
  (gethash output-id ewl-workspace-current))

(defun ewl-workspace-switch-on-output (output-id workspace-index)
  "Switch to WORKSPACE-INDEX on OUTPUT-ID."
  (interactive)
  (let* ((workspace-frames (gethash output-id ewl-workspace-list))
         (current-index (ewl-workspace-get-current-index output-id))
         (target-frame (ewl-workspace-get-output-frame output-id))
         (current-output-id (ewl-workspace-get-current-output-id))
         (same-workspace-p (= workspace-index current-index))
         (different-output-p (not (= output-id current-output-id))))

    (when (and workspace-frames
               (>= workspace-index 0)
               (< workspace-index (length workspace-frames)))

      (cond
       ;; Case 1: Same workspace on different output - just switch focus
       ((and same-workspace-p different-output-p)
        (when (and target-frame (not (eq target-frame (selected-frame))))
          (let ((frame-surface (frame-parameter target-frame 'ewl-surface)))
            (when frame-surface
              (ewc-request frame-surface 'focus)
              (select-frame-set-input-focus target-frame)
              (message "Switched focus to output %d (workspace %d already active)" output-id workspace-index))
            (unless frame-surface
              (select-frame-set-input-focus target-frame)
              (message "Switched focus to output %d (no wayland surface)" output-id)))))

       ;; Case 2: Different workspace - do full workspace switch
       ((not same-workspace-p)
        (message "Switching output %d from workspace %d to %d"
                 output-id current-index workspace-index)

        ;; Hide surfaces from current workspace
        (ewl-workspace-hide-surfaces output-id current-index)

        ;; Update current workspace
        (puthash output-id workspace-index ewl-workspace-current-index)
        (puthash output-id (nth workspace-index workspace-frames) ewl-workspace-current)

        ;; Show surfaces for new workspace
        (ewl-workspace-show-surfaces output-id workspace-index)

        ;; Switch focus to the target output if different from current
        (when (and target-frame different-output-p (not (eq target-frame (selected-frame))))
          (let ((frame-surface (frame-parameter target-frame 'ewl-surface)))
            (when frame-surface
              (ewc-request frame-surface 'focus)
              (select-frame-set-input-focus target-frame)
              (message "Switched focus to output %d via wayland surface" output-id))
            (unless frame-surface
              (select-frame-set-input-focus target-frame)
              (message "Switched focus to output %d (no wayland surface)" output-id))))

        ;; Run hook
        (run-hooks 'ewl-workspace-switch-hook))

       ;; Case 3: Same workspace on same output - do nothing
       (t
        (message "Already on workspace %d on output %d" workspace-index output-id))))))

(defvar ewl-workspace-window-configurations (make-hash-table :test 'equal)
  "Hash table mapping (output-id . workspace-index) to window configurations.")

(defvar ewl-workspace-frame-assignments (make-hash-table :weakness 'key)
  "Hash table mapping frame to assigned output-id.")

(defun ewl-workspace-assign-frame-to-output (frame output-id)
  "Assign FRAME to OUTPUT-ID for workspace management."
  (puthash frame output-id ewl-workspace-frame-assignments)
  (set-frame-parameter frame 'ewl-assigned-output-id output-id))

(defun ewl-workspace-get-frame-output-id (frame)
  "Get the output-id assigned to FRAME, assigning one if needed."
  (or (frame-parameter frame 'ewl-output-id)
      (gethash frame ewl-workspace-frame-assignments)
      (frame-parameter frame 'ewl-assigned-output-id)
      ;; Auto-assign frames without output-id to output 0
      (progn
        (ewl-workspace-assign-frame-to-output frame 0)
        0)))

(defun ewl-workspace-get-output-frames (output-id)
  "Get all frames assigned to OUTPUT-ID."
  (cl-remove-if-not (lambda (frame)
                      (= output-id (ewl-workspace-get-frame-output-id frame)))
                    (frame-list)))

(defun ewl-workspace-get-output-frame (output-id)
  "Get the primary frame associated with OUTPUT-ID."
  (or (cl-find-if (lambda (frame)
                    (eq output-id (frame-parameter frame 'ewl-output-id)))
                  (frame-list))
      (car (ewl-workspace-get-output-frames output-id))))

(defun ewl-workspace-get-frame-buffers (frame)
  "Get all buffers currently visible in FRAME's windows."
  (with-selected-frame frame
    (mapcar #'window-buffer (window-list frame))))

(defun ewl-workspace-save-current-configuration (output-id workspace-index)
  "Save the current window configuration for all frames on OUTPUT-ID WORKSPACE-INDEX."
  (let* ((frames (ewl-workspace-get-output-frames output-id))
         (key (cons output-id workspace-index))
         (frame-configs '()))
    (dolist (frame frames)
      (with-selected-frame frame
        (push (cons frame (current-window-configuration)) frame-configs)))
    (when frame-configs
      (puthash key frame-configs ewl-workspace-window-configurations)
      (message "Saved window configuration for %d frames on output %d workspace %d"
               (length frame-configs) output-id workspace-index))))

(defun ewl-workspace-restore-configuration (output-id workspace-index)
  "Restore saved window configuration to all frames on OUTPUT-ID WORKSPACE-INDEX."
  (let* ((key (cons output-id workspace-index))
         (saved-configs (gethash key ewl-workspace-window-configurations))
         (frames (ewl-workspace-get-output-frames output-id)))
    (if saved-configs
        (progn
          ;; Restore window configurations for each frame
          (dolist (frame-config saved-configs)
            (let ((frame (car frame-config))
                  (config (cdr frame-config)))
              (when (frame-live-p frame)
                (with-selected-frame frame
                  (set-window-configuration config)))))
          (message "Restored window configuration for output %d workspace %d"
                   output-id workspace-index))
      ;; No saved configuration, create default single window with workspace buffer
      (dolist (frame frames)
        (when (frame-live-p frame)
          (with-selected-frame frame
            (delete-other-windows)
            (let ((workspace-buffer (ewl-workspace-output-get-workspace output-id)))
              (switch-to-buffer workspace-buffer)))))
      (message "Created default configuration for output %d workspace %d"
               output-id workspace-index))))

(defun ewl-workspace-hide-surfaces (output-id workspace-index)
  "Hide all surfaces assigned to WORKSPACE-INDEX on OUTPUT-ID."
  (ewl-workspace-save-current-configuration output-id workspace-index)
  ;; Hide wayland surfaces - TODO: integrate with ewl-hide when surfaces are tracked
  (message "Hiding surfaces for output %d workspace %d" output-id workspace-index))

(defun ewl-workspace-show-surfaces (output-id workspace-index)
  "Show all surfaces assigned to WORKSPACE-INDEX on OUTPUT-ID."
  (ewl-workspace-restore-configuration output-id workspace-index)
  ;; Show wayland surfaces - TODO: integrate with ewl-layout when surfaces are tracked
  (message "Showing surfaces for output %d workspace %d" output-id workspace-index))

(defun ewl-workspace-assign-surface (surface output-id workspace-index)
  "Assign SURFACE to WORKSPACE-INDEX on OUTPUT-ID."
  (puthash surface (cons output-id workspace-index) ewl-workspace-surface-assignments)
  (message "Assigned surface to output %d workspace %d" output-id workspace-index))

(defun ewl-workspace-get-surface-assignment (surface)
  "Get the (OUTPUT-ID . WORKSPACE-INDEX) assignment for SURFACE."
  (gethash surface ewl-workspace-surface-assignments))

;;; Helper Functions for User Commands

(defun ewl-workspace-get-current-output-id ()
  "Get the output-id for the currently selected frame."
  (or (frame-parameter (selected-frame) 'ewl-output-id)
      (ewl-workspace-get-frame-output-id (selected-frame))
      0))

;;; User Commands

(defun ewl-workspace-switch-to-0 ()
  "Switch to workspace 0 on current output."
  (interactive)
  (ewl-workspace-switch-on-output (ewl-workspace-get-current-output-id) 0))

(defun ewl-workspace-switch-to-1 ()
  "Switch to workspace 1 on current output."
  (interactive)
  (ewl-workspace-switch-on-output (ewl-workspace-get-current-output-id) 1))

(defun ewl-workspace-switch-to-2 ()
  "Switch to workspace 2 on current output."
  (interactive)
  (ewl-workspace-switch-on-output (ewl-workspace-get-current-output-id) 2))

(defun ewl-workspace-switch-to-3 ()
  "Switch to workspace 3 on current output."
  (interactive)
  (ewl-workspace-switch-on-output (ewl-workspace-get-current-output-id) 3))

;;; Configuration and Key Bindings

(defcustom ewl-workspace-keybindings
  '((output-0 . ((1 . 0) (2 . 1) (3 . 2) (4 . 3)))
    (output-1 . (("-" . 0) ("=" . 1) ("\\" . 2) ("`" . 3))))
  "Keybinding configuration for workspace switching.
An alist where each element is (OUTPUT-SYMBOL . BINDINGS).
BINDINGS is an alist of (KEY-STRING . WORKSPACE-INDEX) pairs.
KEY-STRING can be a number (converted to string) or a symbolic key.

For example:
- s-1 through s-4 switch to workspaces 0-3 on output 0
- s-- s-= s-\\ s-` switch to workspaces 0-3 on output 1"
  :type '(alist :key-type symbol
                :value-type (alist :key-type (choice string integer) :value-type integer))
  :group 'ewl-workspace)

(defun ewl-workspace-setup-keybindings ()
  "Set up workspace switching keybindings based on configuration."
  (interactive)
  ;; Collect all keys from current configuration for cleanup
  (let ((all-keys '()))
    (dolist (output-config ewl-workspace-keybindings)
      (dolist (binding (cdr output-config))
        (let* ((key-spec (car binding))
               (key-string (if (numberp key-spec)
                               (number-to-string key-spec)
                             key-spec)))
          (push key-string all-keys))))

    ;; Clear existing keybindings from configuration
    (dolist (key-string all-keys)
      (global-unset-key (kbd (format "s-%s" key-string))))

    ;; Set up new keybindings
    (dolist (output-config ewl-workspace-keybindings)
      (let ((output-symbol (car output-config))
            (bindings (cdr output-config)))
        (dolist (binding bindings)
          (let* ((key-spec (car binding))
                 (workspace-index (cdr binding))
                 (output-id (pcase output-symbol
                              ('output-0 0)
                              ('output-1 1)
                              (_ (string-to-number (substring (symbol-name output-symbol) -1)))))
                 (key-string (if (numberp key-spec)
                                 (number-to-string key-spec)
                               key-spec))
                 (key-binding (kbd (format "s-%s" key-string))))
            (global-set-key key-binding
                            `(lambda ()
                               (interactive)
                               (ewl-workspace-switch-on-output ,output-id ,workspace-index)
                               (message "Switched to workspace %d on output %d"
                                        ,workspace-index ,output-id)))))))

    ;; Display configured keybindings
    (message "EWL workspace keybindings configured: %s"
             (mapconcat (lambda (config)
                          (format "%s: s-%s"
                                  (car config)
                                  (mapconcat (lambda (binding)
                                               (let ((key-spec (car binding)))
                                                 (if (numberp key-spec)
                                                     (number-to-string key-spec)
                                                   key-spec)))
                                             (cdr config) ",")))
                        ewl-workspace-keybindings " | "))))

(defun ewl-workspace-add-output-keybindings (output-id start-key)
  "Add keybindings for OUTPUT-ID starting from START-KEY.
For example: (ewl-workspace-add-output-keybindings 2 9)
would bind s-9, s-0 for workspaces 0-1 on output 2."
  (let ((bindings '()))
    (dotimes (i ewl-workspace-number)
      (let ((key-num (+ start-key i)))
        (when (<= key-num 9)
          (push (cons key-num i) bindings))))
    (when (> (+ start-key ewl-workspace-number) 9)
      ;; Wrap around to 0 if needed
      (dotimes (i (- (+ start-key ewl-workspace-number) 10))
        (push (cons i (+ (- 9 start-key) 1 i)) bindings)))
    (add-to-list 'ewl-workspace-keybindings
                 (cons (intern (format "output-%d" output-id)) (reverse bindings)))
    (ewl-workspace-setup-keybindings)))

(provide 'ewl-workspace)
;;; ewl-workspace.el ends here
