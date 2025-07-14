;;; ewx.el --- Emacs Wayland X - Wayland Compositor inside Emacs.
;;
;; Filename: ewx.el
;; Description: Main entry file.
;; Author: Slava Barinov
;; Maintainer: Slava Barinov <rayslava@rayslava.com>
;; Created: Mon Jul 14 13:07:31 2025 (+0900)
;; Version: 0.1.0
;; Package-Requires: (("emacs" . "29.1"))
;; URL: https://github.com/rayslava/ewx
;; Doc URL: https://github.com/rayslava/ewx
;; Keywords: wayland
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file is the main entry point for running the compositor.
;;
;; The actual work is done in other files.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ewl)

(provide 'ewx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ewx.el ends here
