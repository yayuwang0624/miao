;;; miao-core.el --- Miao variables  -*- lexical-binding: t; -*-

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

(require 'subr-x)
(require 'cl-lib)

(defun miao--switch-state (state)
  "Switch to STATE execute 'miao-switch-state-hook unless NO-HOOK is non-nil."
  (let* ((state (if (alist-get state miao-state-mode-alist) state 'normal))
         (mode (alist-get state miao-state-mode-alist)))
    (unless (eq state (miao--current-state))
      (funcall mode 1))))

(defun miao--leader-cleanup ()
  (setq miao--leader-keys nil
        miao--prefix-arg nil
        miao--leader-keymap-description-activated nil)
  (when (eq overriding-local-map miao-leader-base-keymap)
    (setq overriding-local-map nil))
  (when (fboundp 'which-key--hide-popup)
    (which-key--hide-popup)))

(defun miao--leader-recover ()
  (miao--leader-cleanup)
  (when (bound-and-true-p miao-leader-mode)
    (miao-leader-mode -1))
  (let ((cmd (key-binding (this-command-keys-vector) t)))
    (when (and (commandp cmd)
               (not (eq cmd 'miao-leader-self-insert)))
      (setq this-command cmd
            real-this-command cmd)
      (call-interactively cmd))))

(defun miao--current-state ()
  miao--current-state)

(defun miao-switch-to-previous-state ()
  (miao--switch-state miao--leader-previous-state))

(defun miao--event-key (event)
  (let ((c (event-basic-type event)))
    (if (and (char-or-string-p c)
             (member 'shift (event-modifiers event)))
        (upcase c)
      c)))

(defun miao--toggle-bypass-mode ()
  (interactive)
  (if (bound-and-true-p miao-bypass-mode)
      (miao-normal-mode)
    (miao-bypass-mode)))

(defun miao-self-insert-command (N &optional C)
  (interactive "p")
  (self-insert-command N C))

(defun miao--parse-input-event (event)
  "Convert EVENT into a normalized key string for leader mode."
  (let ((key-str (key-description event)))
    (cond
     ;; Tab
     ((member key-str '("<tab>" "TAB" "C-i")) "TAB")
     ;; Return / Enter
     ((member key-str '("<return>" "RET" "C-m")) "RET")
     ;; Backspace / DEL
     ((member key-str '("<backspace>" "DEL" "C-h")) "DEL")
     ;; Escape
     ((member key-str '("<escape>" "ESC" "C-["))
      "ESC")
     ;; Space
     ((member key-str '("SPC" " ")) "SPC")
     (t key-str))))

(defun miao-leader-self-insert ()
  "Default command when leader state is enabled."
  (interactive)
  (setq this-command last-command)
  (if (not (eq miao--current-state 'leader))
      (miao--leader-recover)
    (when-let ((event (miao--event-key last-input-event))
               (key (miao--parse-input-event (this-command-keys-vector))))
      (push (cons 'literal key) miao--leader-keys)
      ;; Try execute if the input is valid.
      (miao--leader-try-execute))))

(defun miao--leader-lookup-key (keys)
   (let* ((leader-major-keymap
           (or (gethash major-mode miao-leader-major-keymap-hash)
               miao-leader-state-keymap))
          (keybind
           (when leader-major-keymap
             (lookup-key leader-major-keymap keys))))
     keybind))

(defun miao--leader-describe-keymap (keymap)
  (when (or
         miao--leader-keymap-description-activated
         (setq miao--leader-keymap-description-activated
               (sit-for miao-leader-describe-delay t)))
    (which-key--create-buffer-and-show nil keymap nil (concat "Miao: " (miao--leader-format-keys nil)))))

(defun miao--leader-try-execute ()
  "Try execute command.

If there is a command available on the current key binding,
try replacing the last modifier and try again."
  (let* ((key-str (miao--leader-format-keys nil))
         (cmd (miao--leader-lookup-key (read-kbd-macro key-str))))
    (cond
     ((commandp cmd t)
      (setq current-prefix-arg miao--prefix-arg
            miao--prefix-arg nil
            real-this-command cmd
            this-command cmd)
      (miao-leader-quit)
      (call-interactively cmd))
     ((keymapp cmd)
      (miao--leader-describe-keymap cmd))
     (t
      (setq miao--prefix-arg nil)
      (message "[Miao] %s is undefined" (miao--leader-format-keys nil))
      (miao-leader-quit)))))

(defun miao--leader-format-single-key (key)
  "Return a display format for input KEY."
  (cl-case (car key)
    (literal (cdr key))))

(defun miao--leader-format-upcase (k)
  "Return S-k for upcase k."
  (let ((case-fold-search nil))
    (if (and (stringp k)
             (string-match-p "^[A-Z]$" k))
        (format "S-%s" (downcase k))
      k)))

(defun miao--leader-format-keys (&optional prompt)
  "Return a display format for current input keys."
  (let ((result ""))
    (setq result
          (thread-first
            (mapcar #'miao--leader-format-single-key miao--leader-keys)
            (reverse)
            (string-join " ")))
    (cond
     (prompt
      (setq result (concat result " C-"))))
    result))

(defun miao--prepare-face (&rest _ignore)
  (set-face-attribute 'miao-miacro-fake-cursor
                      nil
                      :background (mix-colors (face-background 'cursor nil t)
                                         (face-background 'region nil t) 0.6))
  (set-face-attribute 'miao-miacro-fake-symbol
                      nil
                      :background (mix-colors (face-background 'cursor nil t)
                                              (face-background 'region nil t) 0.8)))


(provide 'miao-core)
