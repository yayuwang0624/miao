;;; miao-mode.el --- Miao variables  -*- lexical-binding: t; -*-

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

(defmacro miao--state-mode-p (name)
  `(defun ,(miao--intern name "-p") ()
     (bound-and-true-p ,(miao--intern name))))

(defun miao--intern (name &optional p)
  (intern (concat "miao-" (symbol-name name) "-mode" p)))

(defun miao--set-cursor-type (type)
  (if (display-graphic-p)
      (setq cursor-type type)
    (let* ((shape (or (car-safe type) type))
           (param (cond ((eq shape 'bar) "6")
                        ((eq shape 'hbar) "4")
                        (t "2"))))
      (send-string-to-terminal (concat "\e[" param " q")))))

(define-minor-mode miao-mode
  "Miao mode"
  :lighter " Miao"
  :keymap miao-mode-keymap
  (if miao-mode
      (progn
        (if (member major-mode miao-bypass-mode-list)
            (miao-bypass-mode)
          (miao-normal-mode t)))
    (miao--disable-current-mode)))

;;;###autoload
(define-global-minor-mode miao-global-mode miao-mode
  (lambda ()
    (unless (minibufferp)
      (miao-mode 1)))
  (miao--prepare-face)
  (add-to-ordered-list 'emulation-mode-map-alists
                       `((miao-normal-mode . ,miao-normal-state-keymap)))
  (add-to-ordered-list 'emulation-mode-map-alists
                       `((miao-leader-mode . ,miao-leader-base-keymap))))

(defun miao--disable-current-mode ()
  (when miao--current-state
    (funcall (miao--intern miao--current-state) -1)))

(define-minor-mode miao-normal-mode
  "Miao normal mode"
  :lighter " N"
  :keymap miao-normal-state-keymap
  (if miao-normal-mode
      (if (not (equal miao--current-state 'normal))
          ;; switch to normal mode: disable current + set normal
          (progn
            (miao--disable-current-mode)
            (setq miao--current-state 'normal)
            (setq cursor-type 'box)))
    (setq miao--current-state nil)))

(miao--state-mode-p normal)

(define-minor-mode miao-insert-mode
  "Miao insert mode"
  :lighter " I"
  :keymap miao-insert-state-keymap
  (if miao-insert-mode
      (if (not (equal miao--current-state 'insert))
          ;; switch to insert mode: disable current + set insert
          (progn
            (miao--disable-current-mode)
            (setq miao--current-state 'insert)
            (setq cursor-type 'bar)))
    (setq miao--current-state nil)))

(miao--state-mode-p insert)

(define-minor-mode miao-leader-mode
  "Miao leader mode"
  :lighter " L"
  :keymap miao-leader-base-keymap
  (setq miao--leader-previous-state miao--current-state)
  (if miao-leader-mode
      (if (not (equal miao--current-state 'leader))
          ;; switch to leader mode: disable current + set leader
          (progn
            (miao--disable-current-mode)
            (setq miao--current-state 'leader)
            (miao--leader-describe-keymap miao-leader-state-keymap)
            (setq overriding-local-map miao-leader-base-keymap
                  overriding-terminal-local-map nil)))
    (setq miao--current-state nil)))

(define-minor-mode miao-bypass-mode
  "Miao bypass mode"
  :lighter " B"
  :keymap miao-bypass-state-keymap
  (setq miao--bypass-previous-state miao--current-state)
  (if miao-bypass-mode
      (when (not (equal miao--current-state 'bypass))
        ;; switch to bypass mode: disable current + set bypass
        (miao--disable-current-mode)
        (setq miao--current-state 'bypass)
        (let ((keymap (gethash major-mode miao-bypass-keymap-hash)))
          (unless keymap
            (setq keymap (make-sparse-keymap))
            (suppress-keymap keymap t)
            (dolist (chr miao-bypass-mode-keys)
              (let* ((chr-vec (vector (char-to-string chr)))
                     (miao-func (lookup-key miao-normal-state-keymap chr-vec))
                     (major-func (lookup-key (current-local-map) chr-vec)))
                (if major-func
                    (define-key keymap (char-to-string chr) major-func)
                  (if miao-func
                    (define-key keymap (char-to-string chr) miao-func)))))
            (puthash major-mode keymap miao-bypass-keymap-hash))
          (add-to-list 'minor-mode-overriding-map-alist `(miao-mode . ,keymap))))

    (setq miao--current-state nil)))

(provide 'miao-mode)
