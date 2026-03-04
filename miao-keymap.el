;;; miao-keymap.el --- Miao variables  -*- lexical-binding: t; -*-

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

(defvar miao-mode-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c ESC") 'miao-normal-mode)
    (define-key keymap (kbd "M-<SPC>") 'miao-leader-mode)
    keymap))

(defvar miao-normal-state-keymap
  (let ((keymap (make-sparse-keymap)))
    (suppress-keymap keymap t)
    (let ((symbols (append (number-sequence ?! ?/)
                           (number-sequence ?: ?@)
                           (number-sequence ?\[ ?`)
                           (number-sequence ?\{ ?~))))
      (dolist (key symbols)
        (define-key keymap (kbd (char-to-string key)) #'miao-self-insert-command)))
    (define-key keymap (kbd "i") 'miao-insert-mode)
    (define-key keymap (kbd "I") 'miao-insert-end)
    (define-key keymap (kbd "a") 'miao-insert-mode)
    (define-key keymap (kbd "A") 'miao-insert-begin)
    (define-key keymap (kbd "j") 'next-line)
    (define-key keymap (kbd "k") 'previous-line)
    (define-key keymap (kbd "h") 'left-char)
    (define-key keymap (kbd "l") 'right-char)
    (define-key keymap (kbd "<SPC>") 'miao-leader-mode)
    (define-key keymap (kbd "C-z") 'miao--toggle-bypass-mode)
    keymap)
  "Keymap for Miao normal state.")

(defvar miao-insert-state-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<escape>") 'miao-normal-mode)
    keymap)
  "Keymap for Miao insert state.")

(defvar miao-leader-base-keymap
  (let ((keymap (make-sparse-keymap)))
    ;; (suppress-keymap keymap t)
    (define-key keymap [remap self-insert-command] 'miao-leader-self-insert)
    (define-key keymap [remap keyboard-quit] 'miao-leader-quit)
    (define-key keymap [t] 'miao-leader-self-insert)
    (dolist (key miao-leader-mode-keys)
      (define-key keymap (kbd key) 'miao-leader-self-insert)
      (define-key keymap (kbd (concat "C-" key)) 'miao-leader-self-insert)
      (define-key keymap (kbd (concat "M-" key)) 'miao-leader-self-insert)
      (define-key keymap (kbd (concat "C-M-" key)) 'miao-leader-self-insert))
    (define-key keymap (kbd "<escape>") 'miao-leader-quit)
    (define-key keymap (kbd "C-g") 'miao-leader-quit)
   keymap)
  "Keymap for Miao leader state.")

(defvar miao-leader-state-keymap
  (let ((keymap (make-sparse-keymap)))
    (suppress-keymap keymap t)
    keymap)
  "Keymap for Miao leader state.")

(defvar miao-bypass-state-keymap
  (let ((keymap (make-sparse-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "SPC") 'miao-leader-mode)
    (define-key keymap (kbd "C-z") 'miao--toggle-bypass-mode)
    (define-key keymap (kbd "M-<SPC>") 'miao-leader-mode)
    keymap)
  "Keymap for Miao bypass state.")


(defvar miao-state-mode-alist
  '((normal . miao-normal-mode)
    (insert . miao-insert-mode)
    (leader . miao-leader-mode)
    (bypass  . miao-bypass-mode))
  "Alist of miao states -> modes")

(defvar miao-keymap-alist
  `((normal . ,miao-normal-state-keymap)
    (insert . ,miao-insert-state-keymap)
    (leader . ,miao-leader-state-keymap)
    (bypass  . ,miao-bypass-state-keymap))
  "Alist of symbols of state names to keymaps.")

(provide 'miao-keymap)
