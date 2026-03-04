;;; miao-helpers.el --- Miao variables  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Internal variables and customizable variables.

;;; Code:

(defun miao-define-keys (states &rest keybinds)
  "Define KEYBINDS in STATE.

Example usage:
  (miao-define-keys
    ;; state
    'normal

    ;; bind to a command
    '(\"a\" . miao-append)

    ;; bind to a keymap
    (cons \"x\" ctl-x-map)

    ;; bind to a keybinding which holds a keymap
    '(\"c\" . \"C-c\")

    ;; bind to a keybinding which holds a command
    '(\"q\" . \"C-x C-q\"))"
  (declare (indent 1))
  (if (listp states)
      (dolist (state states)
        (let ((map (alist-get state miao-keymap-alist)))
          (pcase-dolist (`(,key . ,def) keybinds)
            (define-key map (kbd key) def))))
      (let ((map (alist-get states miao-keymap-alist)))
        (pcase-dolist (`(,key . ,def) keybinds)
          (define-key map (kbd key) def)))))


(defun miao-bypass-list-add (mode)
  "Add mode to miao-bypass-mode-list."
  (add-to-list 'mode 'miao-bypass-mode-list))

(defun miao-leader-define-keys (&rest keybinds)
  "Define KEYBINDS in miao leader mode."
  (declare (indent 1))
  (let ((map (alist-get 'leader miao-keymap-alist)))
       (pcase-dolist (`(,key . ,def) keybinds)
         (define-key map (kbd key) def))))

(defun miao-leader-get-major-keymap (major)
  (let ((keymap (gethash major miao-leader-major-keymap-hash)))
    (if keymap
        keymap
      (setq keymap (make-keymap))
      (suppress-keymap keymap t)
      (set-keymap-parent keymap miao-leader-state-keymap)
      (puthash major keymap miao-leader-major-keymap-hash)
      keymap)))

(defun miao-leader-define-major-keys (major &rest keybinds)
  "Define KEYBINDS in miao leader mode."
  (declare (indent 1))
  (let ((map (miao-leader-get-major-keymap major)))
       (pcase-dolist (`(,key . ,def) keybinds)
         (define-key map (kbd key) def))))

(defun miao-indicator ()
  (when (bound-and-true-p miao-global-mode)
    (let* ((state (miao--current-state))
           (state-name (alist-get state miao-modeline-indicators)))
      (if state-name
          (propertize
           (format " %s " state-name)
           'face 'miao-modeline-face)
        ""))))

(provide 'miao-helpers)
